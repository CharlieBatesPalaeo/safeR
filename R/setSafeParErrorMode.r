#' setSafeParErrorMode
#' @description sets the safeR global option.
#' 
#' @param x logical value specifying the safeR option to set. Must be one of TRUE or FALSE.
#' 
#' @details the safeR global option controls the behaviour of safeR validation.
#' If set to FALSE, safeR validation is disabled and all safeR functions will behave as their base R equivalents.
#' 
#' @examples
#' ## set safeR option to strict
#' setSafeParErrorMode("first")
##################
### definition ###
##################
#' @export
setSafeParErrorMode <- function(x) {
    switch(
        x,
        first = options(safeParErrorMode = "first"),
        all = options(safeParErrorMode = "all"),
        stop("typeError: 'x' must be one of 'first' or 'all'")
    )
}
