#' @name validationInfix
#' @title validationInfix (%->%)
#' @description infix operator for function validation
#' 
#' @param lhs a function to be validated
#' @param rhs a character string corresponding to the target class to validate against.
#' 
#' @return validation wrapper of \code{lhs}.
#' 
#' @details Applies both input and output validation to \code{lhs}, with syntax emulating that used
#' for static type checking in languages such as C++, Java, and Rust. Validation is strict, with coercion
#' only permitted where acoercion method between two classes has been defined by the user (for base R classes,
#' default coercion methods are ignored).
#' Syntax for using %->% is as follows:
#' ```
#' valFunc <- (function(
#'     x = xDefault, #: classA
#'     y = yDefault #: classB
#' ) {...}) %->% "classC"
#' ```
#' Here, the '~' pattern is used to signal arguments of \code{lhs} to validate, with the target class specified
#' after the pattern. The target class for output validation is specified, as a character string, after the %->%
#' operator.
#' @examples
## define positive integer class with numeric coercion method
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
#' testWrap <- (function(
#'     x = 1, ~ posInt
#'     y = 2 ~ posInt
#' ) {x + y}) %->% "posInt"
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
#' @rdname validationInfix
#' @aliases %->%
#' @aliases validationInfix,function,generic-method
#' @export
setGeneric(
    '%->%',
    function(
        lhs,
        rhs
    ) {
        stop("typeError: 'lhs' must be a function and 'rhs' must be a character string")
    }
)

# method for function and character string input
# code copied from validateIn and validateOut methods to allow access within R6 classes
#' @rdname validationInfix
#' @aliases %->%
#' @aliases validationInfix,function,character-method
#' @export
setMethod(
    "%->%",
    signature = c(
        lhs = "function",
        rhs = "character"
    ),
    function(
        lhs,
        rhs
    ) {
        # extract input classes from source code of lhs function (discard defaults)
        inCls <- .extractFuncAnnotations(lhs)
        inCls <- stats::setNames(as.list(inCls$class), inCls$object)[!is.na(inCls$class)]

        # formals (strip ~ annotation)
        fmls <- formals(lhs)
        fmls <- lapply(
            fmls,
            function(x) {
                if (is.call(x) && identical(x[[1L]], quote(`~`)) && length(x) == 3L) {
                    x[[2L]]
                } else {
                    x
                }
            }
        )

        # ensure provided args match function signature
        bad <- setdiff(names(inCls), names(fmls))
        if (length(bad)) stop(paste("unknown args: ", paste(bad, collapse = ", ")))

        # ensure classes exist
        missing <- inCls[!vapply(inCls, methods::isClass, logical(1))]
        if (length(missing)) stop(paste("unknown classes: ", paste(missing, collapse = ", ")))

        # create validation wrapper
        wrapper <- rlang::inject(function(...) {
            # pull args from wrapper env
            args <- mget(
                names(formals()),
                envir = environment(),
                inherits = FALSE,
            )
            # lhs function with correct enclosing env
            .target <- !!lhs
            environment(.target) <- environment(sys.function())

            # exit early if safeR validation disabled
            if (!getOption("safeR")) return(res <- do.call(.target, args))
            # validate args
            .inCls <- !!inCls
            for (c in names(.inCls)) {
                # 1) coerce argument to target class
                args[[c]] <- safeR:::.safeRCoerce(
                    args[[c]],
                    .inCls[[c]]
                )
                # 2) addition validation
                methods::validObject(args[[c]])
            }
            print(args)
            # call
            res <- do.call(.target, args)

            # validate output
            res <- safeR:::.safeRCoerce(
                res,
                !!rhs
            )
        })
        # set wrapper formals to match those of func
        formals(wrapper) <- fmls
        return(wrapper)
    }
)

## sub-function for extracting input classes from ~ annotations
.extractFuncAnnotations <- function(
    lhsFunc
) {
    # extract arguments
    args <- as.list(formals(lhsFunc))

    # extract class definitions and defaults
    out <- lapply(
        args,
        function(x) {
            annotated <- is.call(x) &&
                identical(x[[1L]], quote(`~`)) &&
                length(x) == 3L

            list(
                class = if (annotated) as.character(x[[3L]]) else NA_character_,
                default = if (annotated) x[[2L]] else x
            )
        }
    )

    data.frame(
        object = names(out),
        class = vapply(out, `[[`, character(1L), "class"),
        default = I(lapply(out, `[[`, "default")),
        stringsAsFactors = FALSE
    )
}
