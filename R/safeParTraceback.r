#' safeParTraceback
#' 
#' 
##################
### definition ###
##################
#' @export
safeParTraceback <- function() {
    env <- get(".safeRState", envir = asNamespace("safeR"))
    return(env$parErrors)
}