context("events")

test_that("events work and are scoped", {

  # Show how 'on', 'emit' can be used to execute
  # R functions as requested
  counter <- 0
  foo <- function() {
    on("increment", function() counter <<- counter + 1)
    emit("increment")
  }

  # Call once -- increment counter to 1
  foo()
  expect_true(counter == 1)

  # Call again -- increment counter to 2
  foo()
  expect_true(counter == 2)

  # Fire emit with no listener -- no increment
  emit("increment")
  expect_true(counter == 2)

})

test_that("errors in emit handlers don't stop execution", {

  on("ouch", function() {
    stop("failure in handler")
  })

  emit("ouch")
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

  # h2 shouldn't respond when h1 receives emit
  captured <- capture.output(h1$emit("hello"))
  expect_identical(captured, "h1 says hello")

})

test_that("events are run in reverse insertion order", {

  h1 <- new_handlers()

  h1$on("hello", function() cat("hello1\n", sep = ""))
  h1$on("hello", function() cat("hello2\n", sep = ""))

  captured <- capture.output(h1$emit("hello"))
  expect_identical(captured, c("hello2", "hello1"))

})

test_that("handlers can stop event propagation", {

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
    stop_propagation()
    cat("suppressing!\n", sep = "")
  })

  captured <- capture.output(h1$emit("hello"))
  expect_identical(captured, "suppressing!")
})
