
##################
### definition ###
##################
#' @export
setGeneric(
    "easyPar",
    function(
        func,
        iterables
    ) {
        stop("typeError: 'func' must be a function")
    }
)

# method for function
#' @rdname  easyPar
#' @export
setMethod(
    "easyPar",
    signature(func = "function"),
    definition = function(
        func,
        iterables
    ) {
        out <- .getValidationWrapper(
            name = "easyPar",
            func = .easyParInner,
            iterables = .makeIterablesList()
        )(
            func = func,
            iterables = iterables
        )
    }
)

# internal function (actually does things)
.easyParInner <- function(
    func,
    iterables
) {
    # pull fixed args from func definition
    funcArgs <- formals(func)
    fixed <- lapply(names(funcArgs), function(arg) {
        # non-iterables
        if (!(arg %in% names(iterables))) {
            # error if no default
            if (identical(funcArgs[[arg]], quote(expr = ))) {
                stop(paste0("argError: '", arg, "' is missing from iterables"))
            }
            # evaluate default in parent frame
            return(eval(funcArgs[[arg]], envir = parent.frame()))
        }
    })
    print(fixed)
}

# fieldSchema for iterables
.makeIterablesList <- function() {
    new(
        "fieldSchema",
        type = "list",
        validation = function(x) {
            # check x is a list of iterable objects
            all(class(x) == "list" && length(x) > 0)
        }
    ) 
}

### testing
#fixedArg <- 5
#iterArg <- list(1,2,3)

#testFunc <- function(
#    a = fixedArg,
#    b
#) {
#    out <- stats::rnorm(a, b)
#}

#res <- easyPar(
#    func = testFunc,
#    iterables = list(
#        b = iterArg
#    )
#)
