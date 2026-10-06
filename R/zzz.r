####################################################################
### This file contains initialization code for the safeR package ###
####################################################################
.onLoad <- function(
    libname,
    pkgname
) {
    # set the global options
    if (is.null(getOption("safeR"))) {
        options(
            safeR = TRUE
        )
    }
    if (is.null(getOption("safeParErrorMode"))) {
        options(
            safeParErrorMode = "first"
        )
    }
    # create the coercion environment cache within safeR namespace
    if (!exists(".coercionEnvCache", envir = asNamespace("safeR"))) {
        assign(".coercionEnvCache",
            new.env(parent = emptyenv()),
            envir = asNamespace("safeR")
        )
    }
    # create env for storing safePar errors
    assign(".safeRState",
            new.env(parent = emptyenv()),
            envir = asNamespace("safeR")
        )
    invisible()
}