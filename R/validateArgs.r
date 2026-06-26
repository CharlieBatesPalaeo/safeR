#' validateArgs
#' @name validateArgs
#' @docType methods
#' 
#' @description Wrapper for validating function arguments against fieldSchemas. 
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

# method for function
#' @rdname  validateArgs
#' @aliases validateArgs,function-method
#' @method validateArgs function
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
        # ensure schemas match existing arguments
        bad <- setdiff(nms, names(fmls))
        if (length(bad)) stop(paste("unknown args:", paste(bad, collapse = ", ")))

        # define wrapper that validates args
        function(...) {
            caller_env <- parent.frame()
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
                if (!is.null(mc[[a]])) eval(mc[[a]], caller_env)
                # then function default, if not overidden by schema
                else if (!(a %in% nms) && !identical(fmls[[a]], quote(expr = ))) {
                    eval(fmls[[a]], caller_env)
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
                envir = caller_env
            )
        }
    }
)

# method for R6ClassGenerator
#' @rdname  validateArgs
#' @aliases validateArgs,R6ClassGenerator-method
#' @method validateArgs R6ClassGenerator
#' @export
setMethod(
    "validateArgs",
    signature(func = "R6ClassGenerator"),
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

        # extract fields from R6Class
        flds <- list(
            public = names(func$public_fields),
            private = names(func$private_fields)
        )
        # also check initializer for args without defaults
        init <- func$public_methods$initialize
        if (!(is.null(init))) {
            flds$public <- unique(c(
                flds$public,
                .extractInitFields(init)
            ))
        }

        # ensure schemas match existing fields
        bad <- setdiff(nms, unlist(flds))
        if (length(bad)) stop(paste("unknown args:", paste(bad, collapse = ", ")))

        # wrap $new() with validation layer
        function(...) {

        }
    }
)

# helper for extracting fields from initializer
.extractInitFields <- function(init) {
    # init as string
    init_string <- paste(deparse(init), collapse = "\n")
    # extract self$... substrings
    matches <- gregexpr(
        "self\\$[a-zA-Z0-9_]+",
        init_string
    )
    matches <- regmatches(
        init_string,
        matches
    )[[1]]
    # remove prefix
    out <- sub(
        "^self\\$", "",
        matches
    )
}