## temp test that checks package loads
## (might add other tests, but for now example code acts as tests)
test_that("package loads", {
  expect_true(requireNamespace("safeR", quietly = TRUE))
})