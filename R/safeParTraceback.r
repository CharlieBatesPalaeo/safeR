#' safeParTraceback
#' 
#' @description returns a list of error messages from child processes when using safePar()
#' with the global option \code{safeParErrorMode} set to "all".
#' 
#' @examples
## define positive integer class with numeric coercion method
#' setClass(
#'     "posInt",
#'     contains = "integer",
#'     validity = function(object) {
#'         if (object < 0) stop("must be positive") else TRUE
#'     }
#' )
#' setCoercion(
#'     "numeric",
#'     "posInt",
#'     function(x) {
#'         if (length(x) != 1 || !is.numeric(x)) stop("must be a single numeric value")
#'         as(x, "posInt")
#'     }
#' )
#' ## aribtrary function to validate
#' A = "not a posInt"
#' testWrap <- (function(
#'     x = 1 ~ posInt,
#'     y = 2 ~ posInt,
#'     a = A
#' ) {as.numeric(x + y + a)}) %->% "posInt"
#' ## run in parallel
#' testPairwise <- safePar(
#'     func = testWrap,
#'     iterables = list(
#'         x = c(1,2,3),
#'         y = c(4,5,6)
#'     ),
#'     iterType = "pairwise",
#'     nCores = 2
#' )
#' ## view error messages from child processes
#' safeParTraceback()
##################
### definition ###
##################
#' @export
safeParTraceback <- function() {
    env <- get(".safeRState", envir = asNamespace("safeR"))
    return(env$parErrors)
}