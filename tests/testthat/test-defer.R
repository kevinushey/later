context("defer")

emit <- function(...) cat(..., sep = "\n")

test_that("defer pushes handlers to top of stack by default", {

  test <- function() {
    emit("+ foo")
    defer(emit("> foo.1"))
    defer(emit("> foo.2"))
    emit("- foo")
  }

  output <- capture.output(test())
  expected <- c("+ foo", "- foo", "> foo.2", "> foo.1")
  expect_identical(output, expected)

})

test_that("defer can push handlers to bottom of stack", {

  test <- function() {
    emit("+ foo")
    defer(emit("> foo.1"))
    defer(emit("> foo.2"), priority = "last")
    emit("- foo")
  }

  output <- capture.output(test())
  expected <- c("+ foo", "- foo", "> foo.1", "> foo.2")
  expect_identical(output, expected)

})

test_that("errors in defer don't cause terrible things to happen", {

  test <- function() {
    emit("+ foo")
    defer(emit("> foo.1"))
    defer(stop("ouch"))
    defer(emit("> foo.2"))
    emit("- foo")
  }

  output <- capture.output(test())
  expected <- c("+ foo", "- foo", "> foo.2", "> foo.1")
  expect_identical(output, expected)

})

test_that("defer() evaluates expressions in correct frame", {

  x <- 1
  test <- function() {
    y <- 2
    defer(emit(paste(">", x)))
    defer(emit(paste(">", y)))
    defer({
      tryCatch(emit(z), error = function(e) emit(paste(">", 3)))
    })
  }

  output <- capture.output(test())
  expected <- c("> 3", "> 2", "> 1")
  expect_identical(output, expected)
})
