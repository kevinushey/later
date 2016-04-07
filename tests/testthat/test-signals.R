context("signals")

test_that("signals work and are scoped", {

  # Show how 'on', 'signal' can be used to execute
  # R functions as requested
  counter <- 0
  foo <- function() {
    on("increment", function() counter <<- counter + 1)
    signal("increment")
  }

  # Call once -- increment counter to 1
  foo()
  expect_true(counter == 1)

  # Call again -- increment counter to 2
  foo()
  expect_true(counter == 2)

  # Fire signal with no listener -- no increment
  signal("increment")
  expect_true(counter == 2)

})

test_that("errors in signal handlers don't stop execution", {

  on("ouch", function() {
    stop("failure in handler")
  })

  signal("ouch")
  expect_true(TRUE)

})

test_that("the same function can be attached to different handlers", {

  h1 <- new_handlers()
  h2 <- new_handlers()

  h1$on("hello", function() {
    cat("h1 says hello", sep = "")
  })

  h2$on("hello", function() {
    cat("h2 says hello", sep = "")
  })

  # h2 shouldn't respond when h1 receives signal
  captured <- capture.output(h1$signal("hello"))
  expect_identical(captured, "h1 says hello")

})

test_that("signals are run in reverse insertion order", {

  h1 <- new_handlers()

  h1$on("hello", function() cat("hello1\n", sep = ""))
  h1$on("hello", function() cat("hello2\n", sep = ""))

  captured <- capture.output(h1$signal("hello"))
  expect_identical(captured, c("hello2", "hello1"))

})

test_that("signals can return FALSE to stop handler execution", {

  h1 <- new_handlers()

  h1$on("hello", function() {
    cat("shouldn't be emitted", sep = "")
    TRUE
  })

  h1$on("hello", function() {
    cat("shouldn't be emitted", sep = "")
    TRUE
  })

  h1$on("hello", function() {
    cat("suppressing!\n", sep = "")
    FALSE
  })

  captured <- capture.output(h1$signal("hello"))
  expect_identical(captured, "suppressing!")
})
