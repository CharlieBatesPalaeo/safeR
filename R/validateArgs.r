#' validateArgs
#' @name validateArgs
#' @docType methods
#' 
#' @description validateArgs is a wrapper for validating function arguments against fieldSchemas. 
#' 
#' @param func function to wrap with validation layer
#' @param ... named fieldSchema objects corresponding to arguments of \code{func}.
#' 
#' @return a wrapped version of \code{func} that validates its arguments against the provided schemas before executing.
#' 
#' @details Any number of the functions arguments can be validated, but all schemas must match a declared argument of \code{func}.
#' 
#' If an argument is not provided, the \code{fieldSchema} default, if specified, will override the \code{func} default.
#'
#' @examples
#' # define a schema for a positive integer field
#' positiveIntSchema <- new(
#'     "fieldSchema",
#'     type = "numeric",
#'     default = 1,
#'     validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
#' )
#' # arbitrary function 
#' testFunc <- function(a = 1) a + 1
#' # apply validation layer
#' validatedFunc <- validateArgs(
#'     testFunc,
#'     a = positiveIntSchema
#' )
#' # valid args
#' validatedFunc(5)
#' # invalid args
#' try(
#'     validatedFunc("a")
#' )
#' try(
#'     validatedFunc(-3)
#' )
##################
### definition ###
##################
# generic + type error
#' @export
setGeneric(
    "validateArgs",
    function(
        func,
        ...
    ) {
        stop("typeError: 'func' must be a function")
    }
)

# method for fieldSchema
#' @rdname  validateArgs
#' @aliases validateArgs,fieldSchema-method
#' @method validateArgs fieldSchema
#' @export
setMethod(
    "validateArgs",
    signature(func = "function"),
    definition = function(
        func,
        ...
    ) {
        # fast exit if no schemas provided
        schemas <- list(...)
        if (!length(schemas)) return(func)

        # check schemas are valid
        nms <- names(schemas)
        if (is.null(nms) || any(nms == "")) stop("all schemas must be named")
        if (any(!vapply(schemas, inherits, logical(1), "fieldSchema"))) {
            stop("all schemas must be fieldSchema")
        }

        # extract formals (declared arguments) of function
        fmls <- formals(func)
        # ensure schema for all
        bad <- setdiff(nms, names(fmls))
        if (length(bad)) stop(paste("unknown args:", paste(bad, collapse = ", ")))

        # define wrapper that validates args
        function(...) {
            # normalise call (inserts names, orders as in definition, adds default values)
            mc <- match.call(
                func,
                call = as.call(c(
                    list(quote(func)),
                    as.list(substitute(list(...)))[-1]
                )),
                expand.dots = FALSE)
            # validate each argument
            vals <- lapply(
                names(fmls),
                function(a)
            {
                # from call if provided
                if (!is.null(mc[[a]])) eval(mc[[a]], parent.frame())
                # then function default, if not overidden by schema
                else if (!(a %in% nms) && !identical(fmls[[a]], quote(expr = ))) {
                    eval(fmls[[a]], parent.frame())
                }
                # null if using schema default (handled in validateField call below)
                else NULL
            })
            names(vals) <- names(fmls)  
            
            # validate against schemas
            for (a in nms) vals[[a]] <- validateField(vals[[a]], schemas[[a]])
            
            # call original function with validated args
            do.call(
                func,
                vals,
                envir = parent.frame()
            )
        }
    }
)