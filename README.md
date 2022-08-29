# elm-review-no-imported-functions

Provides an [`elm-review`] rule that forbids the use of unqualified functions
from other modules, forcing qualified functions.

[`elm-review`]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/


## Provided rule

- [`NoUnqualifiedFunctions`](NoUnqualifiedFunctions)


## Configuration

```elm
module ReviewConfig exposing (config)

import NoUnqualifiedFunctions
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoUnqualifiedFunctions.rule
    ]
```


## Try it out

```bash
elm-review --template webbhuset/elm-review-no-unqualified-functions/example
```
