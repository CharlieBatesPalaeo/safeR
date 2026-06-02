##################
# validate field #
##################

# generic + type error
setGeneric(
    "validateField",
    function(
        x = NULL,
        schema
    ) {
        stop("typeError: 'schema' must be of class 'fieldSchema'")
    }
)

# method for fieldSchema
setMethod(
    "validateField",
    signature(schema = "fieldSchema"),
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