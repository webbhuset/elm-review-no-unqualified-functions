module ReviewConfig exposing (config)

import NoUnqualifiedFunctions
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnqualifiedFunctions.rule
    ]
