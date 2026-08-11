####################################################################
### This file contains initialization code for the safeR package ###
####################################################################
.onLoad <- function(
    libname,
    pkgname
) {
    # set the safeR global option to strict by default
    if (is.null(getOption("safeR"))) {
        options(safeR = TRUE)
    }
    # create the coercion environment cache within safeR namespace
    .coercionEnvCache <- new.env(parent = emptyenv())
    invisible()
}