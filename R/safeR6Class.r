#' safeR6Class
#' @description creates R6 class with validation logic for annotated fields..
#' 
#' @param classname character string corresponding to the name of the R6 class to create.
#' @param public list of public fields and methods, including explicit reference to annotated fields.
#' @param private list of private fields and methods (see below for potential complications).
#' @param active list of active bindings (see below for potential complications).
#' @param ... additional arguments to pass into [R6::R6Class()] constructor.
#' 
#' @return R6 class with validation logic for annotated fields.
#' 
#' @details annotated fields should be specified in the public list of the R6, while methods should be validated
#' as normal functions, either through [validateIn()] and [validateOut()] or through the infix operator [validationInfix()].
#' 
#' The private and active environments of validated R6 classes contain 'lock box' environments named \code{<fieldName>Locked}
#' and active bindings named \code{<fieldName>} respectively. These are used to store field values and types so that
#' validation logic can be applied to fields when altered within class methods. a call of \code{self$public_fields}
#' will not include annotated fields, but a call of \code{self$active_bindings} will.
#' 
#' An example of R6 validation syntax is as follows:
#' ```
#' valR6 <- safeR6Class(
#'     classname = "valR6",
#'     public = list(
#'         x = xDefault ~ classA,
#'         add = (function(
#'             val ~ ClassB
#'         ) {
#'             self$x <- self$x + val
#'         }) %->% "classC"
#'     )
#' )
#' '''
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
#' ## create R6 class with validation logic
#' testR6 <- safeR6Class(
#'     "testR6",
#'     public = list(
#'         x = 1 ~ posInt,
#'         add = (function(
#'             value = 1 ~ posInt
#'         ) {
#'             self$x <- self$x + value
#'         }) %->% "posInt"
#'     )
#' )
#' ## valid R6 object
#' test <- testR6$new(x = 5)
#' test$x
#' test$add(5)
#' test$x
#' ## invalid method call
#' try(test$add(-5), silent = TRUE)
#' print(.Last.value)
#' ## invalid R6 object
#' try(testR6$new(x = -5), silent = TRUE)
#' print(.Last.value)
#' try(test$add(-5), silent = TRUE)
#' print(.Last.value)


#################
### defintion ###
#################
## public namespace function
# generic + type error
#' @export
setGeneric(
    "safeR6Class",
    function(
        classname = NULL,
        public = list(),
        private = list(),
        active = list(),
        ...
    ) {
        stop("typeError: 'classname' must be a character string")
    }
)

# method for character input
#' @rdname safeR6Class
#' @aliases safeR6Class,character-method
#' @export
setMethod(
    "safeR6Class",
    signature = c(classname = "character"),
    function(
        classname = NULL,
        public = list(),
        private = list(),
        active = list(),
        ...
    ) {
        # create R6 with validation logic
        out <- .safeRClassInner(
            classname = classname,
            public = public,
            publicExpr = substitute(public),
            privateFields = private,
            active = active,
            ...
        )
    }
)

## internal function (actual logic)
.safeRClassInner <- function(
    classname = NULL,
    public = list(),
    publicExpr = NULL,
    privateFields = list(),
    active = list(),
    ...
) {
    # extract annotations and defaults from public list
    metaDat <- .extractListAnnotations(publicExpr)
    # remove methods
    isField <- !vapply(public, is.function, logical(1L))
    metaDat <- metaDat[isField, , drop = FALSE]

    # classes for annotated
    inCls <- stats::setNames(as.list(
        metaDat$class),
        metaDat$object)
    inCls <- inCls[!is.na(inCls)]

    # private 'lock boxes' for annotated fields
    for (c in names(inCls)) {
        # check for namespace conflicts
        if (c %in% names(privateFields)) stop(paste0(
            "namespace conflict: private field '",
            c,
            "' already exists"
        ))
        if (c %in% names(active)) stop(paste0(
            "namespace conflict: active binding '",
            c,
            "' already exists"
        ))
        # create private lock box
        privateFields[[paste0(c, "Locked")]] <- new.env(parent = emptyenv())

        # create R6 active binding
        active[[c]] <- rlang::inject(
            function(lockedObj) {
                if (missing(lockedObj)) {
                    lockedObj <- private[[!!paste0(c, "Locked")]]$val
                } else {
                    if (getOption("safeR")) {
                        # 1) coerce argument to target class
                        lockedObj <- safeR:::.safeRCoerce(
                            lockedObj,
                            private[[!!paste0(c, "Locked")]]$class
                        )
                        # 2) check validity
                        methods::validObject(lockedObj)
                    }
                    # 3) update value
                    private[[!!paste0(c, "Locked")]]$val <- lockedObj
                }
                invisible(lockedObj)
            }
        )
    }
    # generate initialize function to validate annotated fields
    init <- rlang::inject(function(...) {
        # pull args from wrapper env
        args <- mget(
            names(formals()),
            envir = environment(),
            inherits = FALSE,
        )
        # if safeR validation enabled
        # validate args
        .inCls <- !!inCls
        for (c in names(.inCls)) {
            if (getOption("safeR")) {
                # 1) coerce argument to target class
                args[[c]] <- safeR:::.safeRCoerce(
                    args[[c]],
                    .inCls[[c]]
                )
                # 2) addition validation
                methods::validObject(args[[c]])
            }
            # 3) populate lock box
            private[[paste0(c, "Locked")]]$val <- args[[c]]
            private[[paste0(c, "Locked")]]$class <- class(args[[c]])
        }
    })
    # formals
    initFmls <- stats::setNames(metaDat$default, metaDat$object)
    formals(init) <- initFmls

    # add to public
    public[names(inCls)] <- NULL
    public <- c(list(initialize = init), public)

    # create R6 class
    R6::R6Class(
        classname = classname,
        public = public,
        private = privateFields,
        active = active,
        ...
    )
}

## sub-function for extracting input classes from ~ annotations
.extractListAnnotations <- function(
    lhsFunc
) {
    # extract arguments
    args <- as.list(lhsFunc)[-1L]

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