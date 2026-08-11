library(safeR)
setClass("posInt", contains = "integer", validity = function(object) { if (object < 0) stop("must be positive") else TRUE })
setCoercion("numeric", "posInt", function(x) { if (length(x) != 1 || !is.numeric(x)) stop("must be a single numeric value"); as(x, "posInt") })
testWrap <- (function(x = 1, y = 2) {x + y}) %->% "posInt"
print(testWrap())
print(testWrap(x = 5))