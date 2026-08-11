#' fieldSchema
#' @name fieldSchema-class
#'
#' @description implementation of schema for defining fields
#' 
#' @slot type character: expected class of value
#' @slot default any: default value for the field (\code{NULL} if not provided)
#' @slot validation function: validation function for the field (\code{function(x) TRUE} if not provided)
#' 
#' @details
#' Use \code{new("fieldSchema", ...)} to create a new schema. This will run internal validation checks on the type (must be a valid, loaded class),
#' default (must be of the specified type), and validation function (must return a single boolean). See examples below for usage and error cases.
#'  
#' @examples
#' # valid schema
#' positiveInt <- new(
#'     "fieldSchema",
#'     type = "numeric",
#'     default = 1,
#'     validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
#' )
#' 
#' # invalid type
#' try(
#'     new(
#'         "fieldSchema",
#'         type = "notARealClass"
#'     )
#' )
#'
#' # default of incorrect type
#' try(
#'     new(
#'         "fieldSchema",
#'         type = "numeric",
#'         default = "a"
#'     )
#' )
#'
#' # invalid validation function
#' try(
#'     new(
#'         "fieldSchema",
#'         type = "numeric",
#'         validation = function(x) "not a boolean"
#'     )
#' )
##################
### definition ###
##################
#' @exportClass fieldSchema
methods::setClass(
    "fieldSchema",
    slots = c(
        type = "character",
        default = "ANY",
        validation = "function"
    )
)

##############
# validation #
##############
.checkType <- function(type) {
    if (!methods::isClass(type)) {
        stop("invalid fieldSchema: 'type' must be a valid class name")
    }
}

.checkDefault <- function(
    default,
    type
) {
    if (!is.null(default) && !class(default) == type) {
        stop(paste0(
            "invalid fieldSchema: 'default' value must be of class '",
            type,
            "'"
        ))
    }
}

.checkValidation <- function(validation) {
    test <- try(validation(NA), silent = TRUE)
    if (inherits(test, "try-error")) {
        stop("invalid fieldSchema: 'validation' function failed")
    }

    if (!is.logical(test) || length(test) != 1) {
        stop("invalid fieldSchema: 'validation' function must return a single boolean")
    }
}

methods::setValidity(
    "fieldSchema",
    function(object) {
        .checkType(object@type)
        .checkDefault(
            object@default,
            object@type
        )
        .checkValidation(object@validation)
        return(TRUE)
    }
)

########
# init #
########
methods::setMethod(
    "initialize",
    "fieldSchema",
    function(
        .Object,
        type,
        default = NULL,
        validation = function(x) TRUE
    ) {
        # parse args
        .Object@type <- type
        .Object@default <- default
        .Object@validation <- validation
        # validate
        methods::validObject(.Object)
        # return
        return(.Object)
    }
)
