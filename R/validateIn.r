#' validateIn
#' @description creates wrapper for functions that validates inputs against class definitions.
#' 
#' @param func function to wrap with validation layer.
#' @param ... named character strings corresponding to arguments of \code{func} and the
#' target class to validate against.
#' 
#' @return validation wrapper of \code{func}.
#' 
#' @details Any number of the arguments can be validated, but all target classes must match a declared
#' argument of \code{func}. Wrapper formals will match those of \code{func}, ignoring class defaults
#' (i.e. prototypes). This is done to avoid issues stemming from the propogation of base R defaults -
#' it is safer to raise errors where necesarry arguments are not provided than assume base R defaults are
#' appropriate.
#' 
#' @examples
#' ## define positive integer class with numeric coercion method
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
#' testFunc <- function(
#'     x= 1,
#'     y = 2) {
#'     print(x + y)
#' }
#' ## apply validation wrapper
#' testWrap <- validateIn(
#'     func = testFunc,
#'     x = "posInt",
#'     y = "posInt"
#' )
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
#' @export
setGeneric(
    "validateIn",
    function(
        func,
        ...
    ) {
        stop("typeError: 'func' must be a function")
    }
)

# method for function input
#' @rdname validateIn
#' @aliases validateIn,function-method
#' @export
setMethod(
    "validateIn",
    signature = c(func = "function"),
    function(
        func,
        ...
    ) {
        # create validation wrapper
        wrapper <- .validateIn(func, ...)
    }
)

## internal function (actual logic)
.validateIn <- function(
    func,
    ...
) {
    fmls <- formals(func)
    cls <- list(...)

    # ensure provided args match function signature
    bad <- setdiff(names(cls), names(fmls))
    if (length(bad)) stop(paste("unknown args: ", paste(bad, collapse = ", ")))

    # ensure classes exist
    missing <- cls[!vapply(cls, methods::isClass, logical(1))]
    if (length(missing)) stop(paste("unknown classes: ", paste(missing, collapse = ", ")))

    # create validation wrapper
    wrapper <- rlang::inject(function(...) {
        # pull args from wrapper env
        args <- mget(
            names(formals()),
            envir = environment(),
            inherits = FALSE,
        )

        # exit early if safeR validation disabled
        if (getOption("safeR")) {
            # validate args
            .cls <- !!cls
            for (c in names(.cls)) {
                # 1) coerce argument to target class
                args[[c]] <- safeR:::.safeRCoerce(
                    args[[c]],
                    .cls[[c]]
                )
                # 2) addition validation
                methods::validObject(args[[c]])
            }
        }
        # call
        res <- do.call(!!func, args)
    })
    # set wrapper formals to match those of func
    formals(wrapper) <- fmls
    return(wrapper)
}
