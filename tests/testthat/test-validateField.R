# Target: validateField.R
# Tests:
# 1) valid value against schema
# 2) invalid type against schema
# 3) value that violates validation function
# 4) default value used when x is NULL
# 5) error thrown when schema not provided

# schema
positiveIntSchema <- methods::new(
    "fieldSchema",
    type = "numeric",
    default = 1,
    validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
)

# valid value
testthat::test_that(
    "valid value against schema", {
        testthat::expect_equal(
            validateField(
                5,
                positiveIntSchema
            ),
            5
        )
    }
)

# invalid type
testthat::test_that(
    "invalid type against schema", {
        testthat::expect_error(
            validateField(
                "a",
                positiveIntSchema
            ),
            regexp = "typeError: value must be of class 'numeric'"
        )
    }
)

# violates validation function
testthat::test_that(
    "value that violates validation function", {
        testthat::expect_error(
            validateField(
                -3,
                positiveIntSchema
            ),
            regexp = "validationError: value does not satisfy validation function for field 'numeric': function(x) is.numeric(x) && length(x) == 1 && x > 0"
        )
    }
)

# default value used when x is NULL
testthat::test_that(
    "default value used when x is NULL", {
        testthat::expect_equal(
            validateField(
                NULL,
                positiveIntSchema
            ),
            1
        )
    }
)

# error thrown when schema not provided
testthat::test_that(
    "error thrown when schema not provided", {
        testthat::expect_error(
            validateField(
                5,
                "notARealSchema"
            ),
            regexp = "typeError: 'schema' must be of class 'fieldSchema'"
        )
    }
)
