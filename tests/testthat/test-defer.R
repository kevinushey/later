context("defer")

put <- function(...) cat(..., sep = "\n")

test_that("defer pushes handlers to top of stack by default", {

  test <- function() {
    put("+ foo")
    defer(put("> foo.1"))
    defer(put("> foo.2"))
    put("- foo")
  }

  output <- capture.output(test())
  expected <- c("+ foo", "- foo", "> foo.2", "> foo.1")
  expect_identical(output, expected)

})

test_that("defer can push handlers to bottom of stack", {

  test <- function() {
    put("+ foo")
    defer(put("> foo.1"))
    defer(put("> foo.2"), priority = "last")
    put("- foo")
  }

  output <- capture.output(test())
  expected <- c("+ foo", "- foo", "> foo.1", "> foo.2")
  expect_identical(output, expected)

})

test_that("errors in defer don't cause terrible things to happen", {

  test <- function() {
    put("+ foo")
    defer(put("> foo.1"))
    defer(stop("ouch"))
    defer(put("> foo.2"))
    put("- foo")
  }

  output <- capture.output(test())
  expected <- c("+ foo", "- foo", "> foo.2", "> foo.1")
  expect_identical(output, expected)

})

test_that("defer() evaluates expressions in correct frame", {

  x <- 1
  test <- function() {
    y <- 2
    defer(put(paste(">", x)))
    defer(put(paste(">", y)))
    defer({
      tryCatch(put(z), error = function(e) put(paste(">", 3)))
    })
  }

  output <- capture.output(test())
  expected <- c("> 3", "> 2", "> 1")
  expect_identical(output, expected)

})

test_that("defer() doesn't attach handlers to global env", {
  expect_error(evalq(defer("ouch"), envir = .GlobalEnv))
})
