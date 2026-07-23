# Target: validateArgs.R
# Tests:
# 1) valid args agains schema
# 2) arg of incorrect type
# 3) arg violating validation function
# 4) default from function when not provided
# 5) schema default override
# 6) non-function input

# schemas
positiveIntSchemaNoDefault <- methods::new(
    "fieldSchema",
    type = "numeric",
    validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
)

positiveIntSchemaDefault <- methods::new(
    "fieldSchema",
    type = "numeric",
    default = 2,
    validation = function(x) is.numeric(x) && length(x) == 1 && x > 0
)

############
# function #
############
testFunc <- function(a = 1) a + 1

# apply validation layer
validatedFuncNoDefault <- validateArgs(
    testFunc,
    a = positiveIntSchemaNoDefault
)

validatedFuncDefault <- validateArgs(
    testFunc,
    a = positiveIntSchemaDefault
)

# valid args
testthat::test_that(
    "valid args for function", {
        testthat::expect_equal(
            validatedFuncNoDefault(5),
            6
        )
    }
)

# incorrect type
testthat::test_that(
    "arg of incorrect type", {
        testthat::expect_error(
            validatedFuncNoDefault("a"),
            regexp = "typeError: value must be of class 'numeric'",
            fixed = TRUE
        )
    }
)

# violates validation function
testthat::test_that(
    "arg violating validation function", {
        testthat::expect_error(
            validatedFuncNoDefault(-3),
            regexp = "validationError: value does not satisfy validation function for field 'numeric': function(x) is.numeric(x) && length(x) == 1 && x > 0",
            fixed = TRUE
        )
    }
)

# default from function when not provided
testthat::test_that(
    "default from function when not provided", {
        testthat::expect_equal(
            validatedFuncNoDefault(),
            2
        )
    }
)

# schema default override
testthat::test_that(
    "schema default override", {
        testthat::expect_equal(
            validatedFuncDefault(),
            3
        )
    }
)

# non-function/R6 input
testthat::test_that(
    "non-function/R6 input", {
        testthat::expect_error(
            validateArgs(
                "notAFunction",
                a = positiveIntSchemaNoDefault
            ),
            regexp = "typeError: 'x' must be a function or R6ClassGenerator",
            fixed = TRUE
        )
    }
)

############
# R6 Class #
############
testR6 <- R6::R6Class(
	"testR6",
	public = list(
	    a = NULL,
	    initialize = function(a = 1) {
		    self$a <- a
		}
    )
)

# apply validation layer
validatedTestNoDefault <- validateArgs(
    testR6,
    a = positiveIntSchemaNoDefault
)

validatedTestDefault <- validateArgs(
    testR6,
    a = positiveIntSchemaDefault
)

# valid args
testthat::test_that(
    "valid args for R6 class", {
        testthat::expect_equal(
            validatedTestNoDefault(5)$a,
            5
        )
    }
)

# incorrect type
testthat::test_that(
    "arg of incorrect type for R6 class", {
        testthat::expect_error(
            validatedTestNoDefault("x"),
            regexp = "typeError: value must be of class 'numeric'",
            fixed = TRUE
        )
    }
)

# violates validation function
testthat::test_that(
    "arg violating validation function for R6 class", {
        testthat::expect_error(
            validatedTestNoDefault(-3),
            regexp = "validationError: value does not satisfy validation function for field 'numeric': function(x) is.numeric(x) && length(x) == 1 && x > 0",
            fixed = TRUE
        )
    }
)

# default from function when not provided
testthat::test_that(
    "default from function when not provided for R6 class", {
        testthat::expect_equal(
            validatedTestNoDefault()$a,
            1
        )
    }
)

# schema default override
testthat::test_that(
    "schema default override for R6 class", {
        testthat::expect_equal(
            validatedTestDefault()$a,
            2
        )
    }
)