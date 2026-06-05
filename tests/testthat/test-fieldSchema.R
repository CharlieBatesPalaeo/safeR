# Target: fieldSchema.R
# Tests:
# 1) schema with valid type, default, and validation function
# 2) schema with invalid type
# 3) schema with default value of incorrect type
# 4) schema with invalid validation function

# valid schema
testthat::test_that(
    "valid schema", {
        positiveInt <- methods::new(
        "fieldSchema",
            type = "numeric",
            default = 1,
            validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
        )
        testthat::expect_s4_class(positiveInt, "fieldSchema")
    }
)

# invalid type
testthat::test_that(
    "invalid type", {
        testthat::expect_error(
            methods::new(
                "fieldSchema",
                type = "notARealClass"
            ),
            regexp = "invalid fieldSchema: 'type' must be a valid class name",
            fixed = TRUE
        )
    }
)

# default of incorrect type
testthat::test_that(
    "default of incorrect type", {
        testthat::expect_error(
            methods::new(
                "fieldSchema",
                type = "numeric",
                default = "a"
            ),
        regexp = "invalid fieldSchema: 'default' value must be of class 'numeric'",
        fixed = TRUE
        )
    }
)

# invalid validation function
testthat::test_that(
    "invalid validation function", {
        testthat::expect_error(
            methods::new(
                "fieldSchema",
                type = "numeric",
                validation = function(x) "not a boolean"
            ),
        regexp = "invalid fieldSchema: 'validation' function must return a single boolean",
        fixed = TRUE
        )
    }
)