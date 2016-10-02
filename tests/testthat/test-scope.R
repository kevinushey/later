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

  test <- function() {
    scope_env_vars(R_LIBS_USER = "nowhere in particular")
    expect_true(Sys.getenv("R_LIBS_USER") == "nowhere in particular")
  }

  before <- Sys.getenv()
  after <- Sys.getenv()

  test()
  expect_identical(before, after)

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

test_that("scope_dir() evaluates handlers in correct order", {

  test <- function() {
    # if this evaluates in wrong order, we might get stuck in tempdir
    scope_dir(tempdir())
    scope_dir(tempdir())
  }

  owd <- getwd()
  test()
  expect_identical(getwd(), owd)
})

test_that("scope_locale() can be used to implement C-locale sorting", {

  sort_c <- function(x) {
    scope_locale(locale = "C")
    sort(x)
  }

  locale <- Sys.getlocale()
  data <- c(0:9, LETTERS, letters)
  expect_identical(sort_c(data), data)
  expect_identical(Sys.getlocale(), locale)
})
