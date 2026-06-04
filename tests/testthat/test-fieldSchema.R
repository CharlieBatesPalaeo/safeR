# Target: fieldSchema.R
# Tests:
# 1) schema with valid type, default, and validation function
# 2) schema with invalid type
# 3) schema with default value of incorrect type
# 4) schema with invalid validation function

# valid schema
testthat::test_that(
  "valid schema",
  {
    positiveInt <- new(
      "fieldSchema",
      type = "numeric",
      default = 1,
      validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
    )
  },
  info = "Creating a fieldSchema with valid type, default, and validation function should succeed."
)

# invalid type
testthat::test_that(
  "invalid type",
  {
    expect_error(
      methods::new(
        "fieldSchema",
        type = "notARealClass"
      ),
      "invalid fieldSchema: 'type' must be a valid class name"
    )
  },
  info = "Creating a fieldSchema with an invalid type should throw an error."
)

# default of incorrect type
testthat::test_that(
  "default of incorrect type",
  {
    expect_error(
      methods::new(
        "fieldSchema",
        type = "numeric",
        default = "a"
      ),
      "invalid fieldSchema: 'default' must be of the specified type"
    )
  },
  info = "Creating a fieldSchema with a default value of incorrect type should throw an error."
)

# invalid validation function
testthat::test_that(
  "invalid validation function",
  {
    expect_error(
      methods::new(
        "fieldSchema",
        type = "numeric",
        validation = function(x) "not a boolean"
      ),
      "invalid fieldSchema: 'validation' must be a function that returns a single boolean"
    )
  },
  info = "Creating a fieldSchema with an invalid validation function should throw an error."
)