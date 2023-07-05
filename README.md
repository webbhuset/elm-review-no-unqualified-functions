# elm-review-no-imported-functions

Provides an [`elm-review`] rule that forbids the use of unqualified functions
from other modules, forcing qualified functions.

[`elm-review`]: https://package.elm-lang.org/packages/jfmengels/elm-review/latest/


## Provided rule

- [`NoUnqualifiedFunctions`](NoUnqualifiedFunctions)


## Automatic Fixes

The rule provides a fix that will add qualifiers to each unqualified function.

Note that this fix will leave the `exposing ()` section of import statements
unchanged; the fixes in [jfmengels/review-unused] can remove them.

[jfmengels/review-unused]: https://package.elm-lang.org/packages/jfmengels/review-unused/latest/NoUnused-Variables


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
