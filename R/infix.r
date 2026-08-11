#' @name validationInfix
#' @title validationInfix (%->%)
#' @description infix operator for function validation
#' 
#' @param lhs a function to be validated
#' @param rhs a character string corresponding to the target class to validate against
#' 
#' @return validation wrapper of \code{lhs}.
#' 
#' @details Applies both input and output validation to \code{lhs}, with syntax emulating that used
#' for static type checking in languages such as C++, Java, and Rust. Validation is strict, with coercion
#' only permitted where acoercion method between two classes has been defined by the user (for base R classes,
#' default coercion methods are ignored).
#' Syntax for using %->% is as follows:
#' ```
#' valFunc <- (function(
#'     x = xDefault, #: classA
#'     y = yDefault #: classB
#' ) {...}) %->% "classC"
#' ```
#' Here, the '#:' pattern is used to signal arguments of \code{lhs} to validate, with the target class specified
#' after the pattern. The target class for output validation is specified, as a character string, after the %->%
#' operator.
#' IMPORTANT: to use %->% in non-interactive mode. For {source()}, set the option `keep.source = TRUE`. For
#' {Rscript} and {R CMD BATCH}, set the environment variables `R_KEEP_SOURCE` amd `R_KEEP_PKG_SOURCE` to "yes".
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
#' testWrap <- (function(
#'     x = 1, #: posInt
#'     y = 2 #: posInt
#' ) {x + y}) %->% "posInt"
#' ## valid args
#' testWrap()
#' testWrap(x = 5)
#' ## invalid args
#' try(testWrap(x = -5), silent = TRUE)
#' print(.Last.value)
#' try(testWrap(x = 5L), silent = TRUE)
#' print(.Last.value)
##################
### definition ###
##################
## public namespace function
# generic + type error
#' @rdname validationInfix
#' @aliases %->%
#' @aliases validationInfix,function,generic-method
#' @export
setGeneric(
    '%->%',
    function(
        lhs,
        rhs
    ) {
        stop("typeError: 'lhs' must be a function and 'rhs' must be a character string")
    }
)

# method for function and character string input
#' @rdname validationInfix
#' @aliases %->%
#' @aliases validationInfix,function,character-method
#' @export
setMethod(
    '%->%',
    signature = c(
        lhs = "function",
        rhs = "character"
    ),
    function(
        lhs,
        rhs
    ) {
        # extract input classes from source code of lhs function
        inCls <- .extractInClasses(lhs)
        inCls <- stats::setNames(as.list(inCls$class), inCls$object)

        # create validation wrapper
        wrapper <- rlang::inject(
            .validateIn(
                func = lhs,
                !!!inCls
            )
        )
        # apply output validation
        wrapper <- .validateOut(wrapper, rhs)
    }
)

## sub-function for extracting input classes from source code of lhs function
.extractInClasses <- function(
    lhsFunc
) {
    # extract arg commnents
    ref <- attr(lhsFunc, "srcref")
    src <- attr(ref, "srcfile")
    lines <- getSrcLines(
        src,
        ref[[1]],
        ref[[3]]
    )
    
    # isolate class definitions
    out <- utils::strcapture(
        "^\\s*(\\w+)\\s*(?:=\\s*[^,]+)?\\s*,?\\s*#:\\s*(\\w+)",
        grep("#:", lines, value = TRUE),
        data.frame(
            object = character(),
            class = character(),
            stringsAsFactors = FALSE
        )
    )
}