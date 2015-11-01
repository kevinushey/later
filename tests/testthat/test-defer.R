context("defer")

emit <- function(x) cat(x, sep = "\n")

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
