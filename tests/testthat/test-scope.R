context("scope")

test_that("scope_dir() sets, restores the working directory", {

  test <- function() {
    dir <- normalizePath(tempdir())
    scope_dir(dir)
    expect_true(normalizePath(getwd()) == dir)
  }

  owd <- getwd()
  test()
  expect_true(getwd() == owd)

})

test_that("scope_env_vars() sets, resets environment variables", {

  test <- function() {
    scope_env_vars(R_LATER_SET = "yes", R_LATER_UNSET = "yes")
    expect_true(Sys.getenv("R_LATER_SET") == "yes")
    expect_true(Sys.getenv("R_LATER_UNSET") == "yes")
  }

  Sys.setenv(R_LATER_SET = "initial")

  test()

  expect_true(Sys.getenv("R_LATER_SET") == "initial")
  expect_true(is.na(Sys.getenv("R_LATER_UNSET", unset = NA)))

})

test_that("scope_options() sets, unsets R options", {

  warn <- getOption("warn")
  options(later.dummy.option = NULL)


  test <- function() {
    expect_true(getOption("warn") == warn)
    scope_options(warn = 0, later.dummy.option = 1)
    expect_true(getOption("warn") == 0)
    expect_true(getOption("later.dummy.option") == 1)
  }

  test()
  expect_true(getOption("warn") == warn)
  expect_null(getOption("later.dummy.option"))

})
