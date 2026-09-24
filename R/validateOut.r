#' validateOut
#' @description creates wrapper for functions that validates outputs against class definitions.
#' 
#' @param func function to wrap with validation layer.
#' @param returnClass character string corresponding to the target class to validate against.
#' 
#' @return validation wrapper of \code{func}.
#' 
#' @details Wrapper formals will match those of \code{func}.
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
#'     x,
#'     y ) {
#'     print(x + y)
#' }
#' ## apply validation wrapper
#' testWrap <- validateOut(
#'     func = testFunc,
#'     returnClass = "posInt"
#' )
#' ## valid args
#' testWrap(x = 5, y = 5)
#' ## invalid args
#' try(testWrap(x = 5L, y = 5L), silent = TRUE)
#' print(.Last.value)
##################
### definition ###
##################
## public namespace function
# generic + type error
#' @export
setGeneric(
    "validateOut",
    function(
        func,
        returnClass
    ) {
        stop("typeError: 'func' must be a function")
    }
)

# method for function input
#' @rdname validateOut
#' @aliases validateOut,function-method
#' @export
setMethod(
    "validateOut",
    signature = c(func = "function"),
    function(
        func,
        returnClass
    ) {
        # create validation wrapper
        wrapper <- .validateOut(func, returnClass)
    }
)

## internal function (actual logic)
.validateOut <- function(
    func,
    returnClass
) {
    fmls <- formals(func)
    # create validation wrapper
    wrapper <- rlang::inject(function(...) {
        # pull args from wrapper env
        args <- tryCatch(
            mget(
                names(formals()),
                envir = environment(),
                inherits = FALSE
            ),
            error = function(e) {
                list()
            }
        )
        # func with correct enclosing env
        .target <- !!func
        environment(.target) <- environment(sys.function())
        # call
        res <- do.call(.target, args)

        # validate output
        if (getOption("safeR")) {
            # 1) check class compatibility (will coerce if possible)
            res <- .safeRCoerce(
                res,
                !!returnClass
            )
            # 2) check validity
            methods::validObject(res)
        }
        return(res)
    })
    # set wrapper formals to match those of func
    formals(wrapper) <- fmls
    return(wrapper)
}