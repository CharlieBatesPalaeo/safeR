#' setSafeParErrorMode
#' @description sets the safeR global option.
#' 
#' @param x character string specifying the error mode. Must be one of "first" or "all".
#' 
#' @details The error mode controls whether [safePar()] stops after the first child-process
#' error or collects errors from all iterations.
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
