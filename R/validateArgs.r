#' validateArgs
#' @name validateArgs
#' @docType methods
#' 
#' @description Wrapper for validating function and R6 class arguments against fieldSchemas. 
#' 
#' @param x function or R6ClassGenerator to wrap with validation layer
#' @param ... named fieldSchema objects corresponding to arguments of \code{x}.
#' 
#' @return a wrapped version of \code{x} that validates its arguments against the provided schemas before executing.
#' 
#' @details Any number of the arguments can be validated, but all schemas must match a declared argument of \code{x}.
#' 
#' If an argument is not provided, the \code{fieldSchema} default, if specified, will override the \code{x} default.
#'
#' @examples
#' ## define a schema for a positive integer field
#' positiveIntSchema <- new(
#'     "fieldSchema",
#'     type = "numeric",
#'     default = 1,
#'     validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
#' )
#' ## arbitrary function 
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
#' ## arbitrary R6 class
#' testR6 <- R6::R6Class(
#'     "testR6",
#'     public = list(
#'         a = NULL,
#'         initialize = function(a = 2) {
#'             self$a <- a
#'         }
#'     )
#' )
#' # apply validation layer
#' validatedR6 <- validateArgs(
#' 	testR6,
#' 	a = positiveIntSchemaDefault
#' )
#' # valid args
#' validatedR6(5)$a
#' # default args (schema overrides)
#' validatedR6()$a
#' # invalid args
#' try(
#'     validatedR6("x")
#' )
#' try(
#'     validatedR6(-3)
#' )
##################
### definition ###
##################
# generic + type error
#' @export
setGeneric(
    "validateArgs",
    function(
        x,
        ...
    ) {
        stop("typeError: 'x' must be a function or R6ClassGenerator")
    }
)

# Register R6 generator class for S4 method signatures.
# R6ClassGenerator is an S3/R6 class, so S4 dispatch needs this bridge.
if (!methods::isClass("R6ClassGenerator")) {
    methods::setOldClass(c("R6ClassGenerator", "R6"))
}

# method for function
#' @rdname  validateArgs
#' @aliases validateArgs,function-method
#' @method validateArgs function
#' @export
setMethod(
    "validateArgs",
    signature(x = "function"),
    definition = function(
        x,
        ...
    ) {
        # fast exit if no schemas provided
        schemas <- list(...)
        if (!length(schemas)) return(x)

        # check schemas are valid
        nms <- names(schemas)
        if (is.null(nms) || any(nms == "")) stop("all schemas must be named")
        if (any(!vapply(schemas, inherits, logical(1), "fieldSchema"))) {
            stop("all schemas must be fieldSchema")
        }

        # extract formals (declared arguments) of function
        fmls <- formals(x)
        # ensure schemas match existing arguments
        bad <- setdiff(nms, names(fmls))
        if (length(bad)) stop(paste("unknown args:", paste(bad, collapse = ", ")))

        # define wrapper that validates args
        function(...) {
            caller_env <- parent.frame()
            # normalise call (inserts names, orders as in definition, adds default values)
            mc <- match.call(
                x,
                call = as.call(c(
                    list(quote(x)),
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
                x,
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
    signature(x = "R6ClassGenerator"),
    definition = function(
        x,
        ...
    ) {
        # fast exit if no schemas provided
        schemas <- list(...)
        if (!length(schemas)) return(x)

        # check schemas are valid
        nms <- names(schemas)
        if (is.null(nms) || any(nms == "")) stop("all schemas must be named")
        if (any(!vapply(schemas, inherits, logical(1), "fieldSchema"))) {
            stop("all schemas must be fieldSchema")
        }

        # extract fields from R6Class
        flds <- list(
            public = names(x$public_fields),
            private = names(x$private_fields)
        )
        # also check initializer for args without defaults
        init <- x$public_methods$initialize
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
            # Capture the caller environment so argument expressions are
            # evaluated where the wrapper was invoked.
            caller_env <- parent.frame()

            # If initialize() exists, we can normalize constructor arguments
            # against its formals and apply schema validation before class
            # instantiation.
            if (!is.null(init)) {
                # Formal initialize arguments (excluding defaults already
                # resolved by match.call below).
                fmls <- formals(init)

                # Rebuild a matched call to initialize() so named and positional
                # arguments are aligned with formal names.
                mc <- match.call(
                    init,
                    call = as.call(c(
                        list(quote(init)),
                        as.list(substitute(list(...)))[-1]
                    )),
                    expand.dots = FALSE
                )

                # Resolve argument values in initialize order:
                # 1) explicit call value
                # 2) initialize default (unless schema should supply default)
                # 3) NULL placeholder to trigger schema default in validateField
                vals <- lapply(
                    names(fmls),
                    function(a)
                {
                    if (!is.null(mc[[a]])) eval(mc[[a]], caller_env)
                    else if (!(a %in% nms) && !identical(fmls[[a]], quote(expr = ))) {
                        eval(fmls[[a]], caller_env)
                    }
                    else NULL
                })
                names(vals) <- names(fmls)

                # Validate only constructor arguments that have a matching schema.
                for (a in intersect(nms, names(vals))) {
                    vals[[a]] <- validateField(vals[[a]], schemas[[a]])
                }

                # Instantiate using validated constructor argument values.
                out <- do.call(
                    x$new,
                    vals,
                    envir = caller_env
                )
            } else {
                # Without initialize(), evaluate provided arguments as-is and
                # forward directly to $new().
                dot_vals <- lapply(
                    as.list(substitute(list(...)))[-1],
                    eval,
                    envir = caller_env
                )

                # Instantiate with raw forwarded arguments.
                out <- do.call(
                    x$new,
                    dot_vals,
                    envir = caller_env
                )
            }

            # Validate resulting object fields after construction so schemas can
            # enforce rules and inject defaults for both public/private fields.
            for (a in nms) {
                if (a %in% flds$public) {
                    # Public fields are directly accessible on the instance.
                    out[[a]] <- validateField(out[[a]], schemas[[a]])
                } else {
                    # Private fields are stored in the R6 private enclosure.
                    out$.__enclos_env__$private[[a]] <- validateField(
                        out$.__enclos_env__$private[[a]],
                        schemas[[a]]
                    )
                }
            }

            # Return validated instance.
            out
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