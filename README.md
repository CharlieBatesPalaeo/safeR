# safeR

`safeR` provides strict type checking tools for R. It supports input and output validation, user-defined coercion methods, validated R6 classes, locked objects, and parallel execution.

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

The [package wiki](https://github.com/CharlieBatesPalaeo/safeR/wiki) contains the complete documentation. It is generated from the package's HTML documentation pages and provides detailed guidance for each exported function and class.
