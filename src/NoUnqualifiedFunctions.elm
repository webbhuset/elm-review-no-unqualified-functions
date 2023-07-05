module NoUnqualifiedFunctions exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Dict.Extra as DE
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import Review.Fix as Fix exposing (Fix)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


type alias Context =
    { currentModuleName : String
    , fixesToApplyByPrefix : Dict String (List Fix)
    , importErrorsByPrefix : Dict String ( { message : String, details : List String }, Range )
    , lookupTable : ModuleNameLookupTable
    , prefixesToApplyByName : Dict String { matchModule : String, prefix : String }
    }


type alias AboutEachImportExpression =
    { errorIngredients : ( { message : String, details : List String }, Range )
    , searchFor : String
    , matchModule : String
    , prefixWith : String
    }


{-| Reports when a module imports functions from other modules.


## Fail

    import Foo exposing (coolFunction)

    localFun =
        coolFunction


## Success

    import Foo

    localFun =
        Foo.coolFunction

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "NoUnqualifiedFunctions" initialContext
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor (importVisitor |> createsNoErrorsHere)
        |> Rule.withExpressionEnterVisitor (createFixesForUsages |> createsNoErrorsHere)
        |> Rule.withFinalModuleEvaluation matchFixesUpToImportErrors
        |> Rule.providesFixesForModuleRule
        |> Rule.fromModuleRuleSchema


createsNoErrorsHere : (Node a -> Context -> Context) -> Node a -> Context -> ( List (Error {}), Context )
createsNoErrorsHere visitor node context =
    ( [], visitor node context )


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { currentModuleName = ""
            , fixesToApplyByPrefix = Dict.empty
            , importErrorsByPrefix = Dict.empty
            , lookupTable = lookupTable
            , prefixesToApplyByName = Dict.empty
            }
        )
        |> Rule.withModuleNameLookupTable


moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor node context =
    ( []
    , { context
        | currentModuleName = Node.value node |> Module.moduleName |> String.join "."
      }
    )


importVisitor : Node Import -> Context -> Context
importVisitor node oldContext =
    case
        Node.value node
            |> .exposingList
            |> Maybe.map Node.value
    of
        Just (Exposing.Explicit nodes) ->
            let
                pairWithErrors :
                    Node Exposing.TopLevelExpose
                    -> List AboutEachImportExpression
                    -> List AboutEachImportExpression
                pairWithErrors exposeNode errors =
                    case Node.value exposeNode of
                        Exposing.FunctionExpose funName ->
                            let
                                nodeValue =
                                    Node.value node

                                importedModule : String
                                importedModule =
                                    nodeValue.moduleName
                                        |> Node.value
                                        |> String.join "."

                                importedAlias =
                                    nodeValue.moduleAlias
                                        |> Maybe.map
                                            (Node.value >> String.join ".")

                                args =
                                    { importModule = importedModule
                                    , importAlias = importedAlias
                                    , functionName = funName
                                    }
                            in
                            (::)
                                { errorIngredients =
                                    ( { message = "Importing functions is not allowed. Use qualified names instead."
                                      , details = [ detailsMessage oldContext args ]
                                      }
                                    , Node.range node
                                    )
                                , searchFor = funName
                                , matchModule = importedModule
                                , prefixWith = importedAlias |> Maybe.withDefault importedModule
                                }
                                errors

                        _ ->
                            errors

                flattenIntoContext :
                    AboutEachImportExpression
                    -> Context
                    -> Context
                flattenIntoContext new accContext =
                    { accContext
                        | importErrorsByPrefix =
                            accContext.importErrorsByPrefix
                                |> Dict.insert new.prefixWith new.errorIngredients
                        , prefixesToApplyByName =
                            accContext.prefixesToApplyByName
                                |> Dict.insert new.searchFor
                                    { matchModule = new.matchModule
                                    , prefix = new.prefixWith
                                    }
                    }
            in
            nodes
                |> List.foldl pairWithErrors []
                |> List.foldl flattenIntoContext oldContext

        _ ->
            oldContext


createFixesForUsages : Node Expression -> Context -> Context
createFixesForUsages node oldContext =
    case Node.value node |> onlyFunctionNames of
        Nothing ->
            oldContext

        Just ( functionModuleName, functionName ) ->
            case oldContext.prefixesToApplyByName |> Dict.get functionName of
                Just { matchModule, prefix } ->
                    let
                        flattenedFunctionModuleName =
                            String.join "." functionModuleName

                        ignoreBecauseAlreadyQualified =
                            functionModuleName /= []

                        ignoreBecauseDifferentModule =
                            Review.ModuleNameLookupTable.moduleNameFor oldContext.lookupTable node
                                == Just functionModuleName
                    in
                    if ignoreBecauseAlreadyQualified then
                        oldContext

                    else if ignoreBecauseDifferentModule then
                        oldContext

                    else
                        {- is not qualified; we should fix it -}
                        let
                            range =
                                Node.range node

                            target =
                                prefix ++ "." ++ functionName

                            combineFixLists : List Fix -> List Fix -> List Fix
                            combineFixLists oldValue newValue =
                                newValue ++ oldValue
                        in
                        { oldContext
                            | fixesToApplyByPrefix =
                                oldContext.fixesToApplyByPrefix
                                    |> DE.insertDedupe combineFixLists prefix [ Fix.replaceRangeBy range target ]
                        }

                Nothing ->
                    oldContext


matchFixesUpToImportErrors : Context -> List (Error {})
matchFixesUpToImportErrors context =
    let
        matchHelper :
            ( String, ( { message : String, details : List String }, Range ) )
            -> Error {}
        matchHelper ( prefix, ingredients ) =
            let
                ( info, range ) =
                    ingredients

                usageFixes : Maybe (List Fix)
                usageFixes =
                    context.fixesToApplyByPrefix |> Dict.get prefix

                fixes =
                    case usageFixes of
                        Just usages ->
                            usages

                        Nothing ->
                            []
            in
            Rule.errorWithFix info range fixes
    in
    context.importErrorsByPrefix
        |> Dict.toList
        |> List.map matchHelper


onlyFunctionNames : Expression -> Maybe ( ModuleName, String )
onlyFunctionNames node =
    case node of
        Application _ ->
            Nothing

        CaseExpression _ ->
            Nothing

        CharLiteral _ ->
            Nothing

        Floatable _ ->
            Nothing

        FunctionOrValue moduleName name ->
            Just ( moduleName, name )

        GLSLExpression _ ->
            Nothing

        Hex _ ->
            Nothing

        IfBlock _ _ _ ->
            Nothing

        Integer _ ->
            Nothing

        LambdaExpression _ ->
            Nothing

        LetExpression _ ->
            Nothing

        ListExpr _ ->
            Nothing

        Literal _ ->
            Nothing

        Negation _ ->
            Nothing

        Operator _ ->
            Nothing

        OperatorApplication _ _ _ _ ->
            Nothing

        ParenthesizedExpression _ ->
            Nothing

        PrefixOperator _ ->
            Nothing

        RecordAccess _ _ ->
            Nothing

        RecordAccessFunction _ ->
            Nothing

        RecordExpr _ ->
            Nothing

        RecordUpdateExpression _ _ ->
            Nothing

        TupledExpression _ ->
            Nothing

        UnitExpr ->
            Nothing


detailsMessage : Context -> { importModule : String, importAlias : Maybe String, functionName : String } -> String
detailsMessage context { importModule, importAlias, functionName } =
    let
        targetName =
            importModule ++ "." ++ functionName

        targetAlias =
            case importAlias of
                Just alias ->
                    " (alias " ++ alias ++ ")"

                Nothing ->
                    ""
    in
    ("'" ++ context.currentModuleName ++ "' imports '" ++ functionName ++ "' from '" ++ importModule ++ "'")
        ++ ("\n Uses of " ++ functionName ++ " should be replaced with " ++ targetName ++ targetAlias ++ ".")
