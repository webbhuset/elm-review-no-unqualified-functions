module NoUnqualifiedFunctions exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Rule, Error)


type alias Context =
    String


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
    Rule.newModuleRuleSchema "NoUnqualifiedFunctions" ""
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


moduleDefinitionVisitor : Node Module -> Context -> ( List (Error {}), Context )
moduleDefinitionVisitor node context =
    ( []
    , Node.value node |> Module.moduleName |> String.join "."
    )


importVisitor : Node Import -> Context -> ( List (Error {}), Context )
importVisitor node currentModule =
    ( case
        Node.value node
            |> .exposingList
            |> Maybe.map Node.value
      of
        Just (Exposing.Explicit nodes) ->
            List.foldl
                (\exposeNode errors ->
                    case Node.value exposeNode of
                        Exposing.FunctionExpose funName ->
                            let
                                importedModule : String
                                importedModule =
                                    Node.value node
                                        |> .moduleName
                                        |> Node.value
                                        |> String.join "."
                            in
                            (::)
                                (Rule.error
                                    { message = "Importing functions is not allowed. Use qualified names instead."
                                    , details = [ detailsMessage currentModule importedModule funName ]
                                    }
                                    (Node.range node)
                                )
                                errors

                        _ ->
                            errors
                )
                []
                nodes

        _ ->
            []
    , currentModule
    )


detailsMessage : String -> String -> String -> String
detailsMessage moduleName importModule functionName =
    "'" ++ moduleName ++ "' imports '" ++ functionName ++ "' from '" ++ importModule ++ "'."
