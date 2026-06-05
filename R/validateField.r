#' validateField
#' @name validateField
#' @docType methods
#' 
#' @description Validate a value against a fieldSchema, returning the value if valid or throwing an error if not.
#' 
#' @param x value to validate (can be NULL)
#' @param schema fieldSchema object defining validation rules
#' 
#' @return validated value
#' 
#' @details This function checks if the provided value \code{x} is valid according to the rules defined in the \code{schema}.
#' 
#' The function first checks if the value is of the correct type as specified in the schema and then applies any uder-defined validation
#' attached to the schema.
#' 
#' If all checks pass, the validated value is returned. Otherwise, an error is thrown.
#' 
#' If \code{x} is \code{NULL}, the default value from the schema is returned (validation will be skipped)
#' 
#' @examples
#' # define a schema for a positive integer field
#' positiveIntSchema <- new(
#'     "fieldSchema",
#'     type = "numeric",
#'     default = 1,
#'     validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
#' )
#' # valid value
#' validateField(
#'     5,
#'     positiveIntSchema
#' )
#' # invalid type
#' try(
#'     validateField(
#'         "a",
#'         positiveIntSchema
#'     )
#' )
#' # violates validation function
#' try(
#'     validateField(
#'         -3,
#'         positiveIntSchema
#'     )
#' )
##################
### definition ###
##################
# generic + type error
#' @export
methods::setGeneric(
    "validateField",
    function(
        x = NULL,
        schema
    ) {
        stop("typeError: 'schema' must be of class 'fieldSchema'")
    }
)

# method for fieldSchema
#' @rdname validateField
#' @aliases validateField,fieldSchema-method
#' @method validateField fieldSchema
#' @export
methods::setMethod(
    "validateField",
    methods::signature(schema = "fieldSchema"),
    definition = function(
        x,
        schema
    ) {
        # return default if x is NULL
        if (is.null(x)) return(schema@default)
        # check type
        if (!inherits(x, schema@type)) {
            stop(paste0(
                "typeError: value must be of class '",
                schema@type,
                "'"
            ))
        }
        # check validation function
        if (!schema@validation(x)) {
            stop(paste0(
                "validationError: value does not satisfy validation function for field '",
                schema@type,
                "': ",
                paste(
                    gsub(
                        "^function\\s*\\(",
                        "function(",
                        trimws(deparse(schema@validation))
                    ),
                    collapse = " "
                )
            ))
        }
        # pass value if valid
        return(x)
    }
)