# Target: lock.R
# Tests:
# 1) lockType locks type but not value
# 2) lockValue locks type and value

# helper functions
.testLockType <- function(
    old,
    new
) {
    # this should fail
    lockType(old)
    old <- new
}

.testLockValue <- function(
    old,
    new
) {
    # this should fail
    lockValue(old)
    old <- new
}

# lockType
x <- 10
testthat::test_that(
    "lockType locks type but not value", {
        # type is locked
        testthat::expect_error(
            .testLockType(x, "notANumeric"),
            regexp = "'old' is locked to type 'numeric'",
        )
        # value is not
        testthat::expect_equal(
            .testLockType(x, 5),
            5
        )
    }
)

# lockValue
y <- 10
testthat::test_that(
    "lockValue locks type and value", {
        # value is locked (type is locked by extension)
        testthat::expect_error(
            .testLockValue(y, 5),
            regexp = "'old' is locked to value of '10'",
        )
    }
)