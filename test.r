library(safeR)
## define positive integer class with numeric coercion method
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
        if (length(x) != 1 || !is.numeric(x)) stop("must be a single numeric value")
        as(x, "posInt")
    }
)
## aribtrary function to validate
testWrap <- (function(
    x = 1, #: posInt
    y = 2 #: posInt
) {x + y}) %->% "posInt"
## valid args
print(testWrap())
print(testWrap(x = 5))