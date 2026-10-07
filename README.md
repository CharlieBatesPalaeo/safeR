# safeR

`safeR` provides strict type checking tools for R. When enabled, validated functions and classes will only execute successfully
if inputs / outputs are of the classes specified by the user, or if the user has explicitely defined a coercion method between the
specified class and the object's class.

[validateIn()](https://github.com/CharlieBatesPalaeo/safeR/wiki/validateIn) and [validateOut()](https://github.com/CharlieBatesPalaeo/safeR/wiki/validateOut) provide validation with typical R syntax, while the [%->%](https://github.com/CharlieBatesPalaeo/safeR/wiki/validationInfix) infix provides an integrated syntax inspired by [Pydantic](https://pydantic.dev/docs/validation/dev/get-started/).

Due to how user-defined validation logic is stored, some extra steps are required for parallelisation. To this end, a [custom parallelisation function](https://github.com/CharlieBatesPalaeo/safeR/wiki/safePar) has been provided. This copies all necessary validation logic (as well as
any required user-defined global objects!) into child processes automatically.

## Installation

```r
remotes::install_github("CharlieBatesPalaeo/safeR")
```

## Quick example

```r
library(safeR)

setClass(
  "posInt",
  contains = "integer",
  validity = function(object) {
    if (object < 0) stop("must be positive") else TRUE
  }
)
setCoercion(
  "numeric",
  "posInt",
  function(x) {
    if (length(x) != 1 || !is.numeric(x)) {
      stop("must be a single numeric value")
    }
    as(x, "posInt")
  }
)
testWrap <- (function(
  x = 1 ~ posInt,
  y = 2 ~ posInt
) { x + y }) %->% "posInt"
testWrap(x = 5)
```

The [package wiki](https://github.com/CharlieBatesPalaeo/safeR/wiki) contains complete documentation. It is generated from the package's HTML documentation pages and provides detailed guidance for each exported namespace object.