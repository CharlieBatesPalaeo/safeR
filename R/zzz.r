####################################################################
### This file contains initialization code for the safeR package ###
####################################################################
.onLoad <- function(
    libname,
    pkgname
) {
    # set the safeR global option to strict by default
    if (is.null(getOption("safeR"))) {
        options(
            safeR = TRUE,
            keep.source = TRUE, 
            keep.source.pkgs = TRUE
        )
    }
    # create the coercion environment cache within safeR namespace
    if (!exists(".coercionEnvCache", envir = asNamespace("safeR"))) {
        assign(".coercionEnvCache",
            new.env(parent = emptyenv()),
            envir = asNamespace("safeR")
        )
    }
    invisible()
}