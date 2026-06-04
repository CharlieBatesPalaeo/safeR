#' validateField
#' 
#' Validate a value against a fieldSchema, returning the value if valid or throwing an error if not.
#' 
#' @param x value to validate (set to NULL to use schema default)
#' @param schema fieldSchema object defining validation rules
#' 
#' @return validated value (default if x is NULL)
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
methods::setMethod(
    "validateField",
    methods::signature(schema = "fieldSchema"),
    definition = function(
        x,
        schema
    ) {
        # default if x is NULL
        if (is.null(x)) x <- schema@default
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
                    trimws(deparse(schema@validation)),
                    collapse = ""
                    )
            ))
        }
        # pass value if valid
        return(x)
    }
)