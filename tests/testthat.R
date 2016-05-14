if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(later)
  test_check("later")
} else {
  warning("'testthat' is not available -- tests will not be run")
}
