#' setSafeR
#' @description sets the safeR global option.
#' 
#' @param x logical value specifying the safeR option to set. Must be one of TRUE or FALSE.
#' 
#' @details the safeR global option controls the behaviour of safeR validation.
#' If set to FALSE, safeR validation is disabled and all safeR functions will behave as their base R equivalents.
#' 
#' @examples
#' ## set safeR option to strict
#' setSafeR(TRUE)
##################
### definition ###
##################
## public namespace function
# generic + type error
#' @export
setGeneric(
    "setSafeR",
    function(x) {
        stop("typeError: 'x' must be a logical value")
    }
)

# method for logical input
#' @rdname setSafeR
#' @aliases setSafeR,logical-method
#' @export
setMethod(
    "setSafeR",
    signature = c(x = "logical"),
    function(x) {
        options(safeR = x)
    }
)