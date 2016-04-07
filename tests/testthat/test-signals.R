context("signals")

test_that("signals work and are scoped", {

  # Show how 'on', 'signal' can be used to execute
  # R functions as requested
  counter <- 0
  foo <- function() {
    on("increment", function() {counter <<- counter + 1})
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
