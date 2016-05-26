#' Create a New Handler Registration
#'
#' Create a new handler registration. Objects generated from this function
#' implement the interface described in \code{\link{events}} -- handlers can be
#' attached and detached with \code{on} / \code{off}. and events can be emitted
#' with \code{emit}.
#'
#' @export
#' @examples
#' # create a handler registration
#' registration <- create_handler_registration()
#'
#' # attach a handler
#' id <- registration$on("hello", function(data) {
#'   print(paste("Hello,", data))
#' }, scoped = FALSE)
#'
#' # emit an event -- see 'Hello, world' printed to console
#' registration$emit("hello", "World")
#'
#' # detach event
#' registration$off("hello", id)
#'
#' # emit event again -- nothing printed as no active handler present
#' registration$emit("hello", "World")
create_handler_registration <- function() {

  # Private ----

  ## Handlers
  .env <- new.env(parent = emptyenv())
  .get_handlers <- function(event) {
    if (!exists(event, envir = .env))
      assign(event, new_list(), envir = .env)
    get(event, envir = .env)
  }

  ## IDs
  .candidates <- c(letters, LETTERS, 0:9)
  .ids <- new.env(parent = emptyenv())

  .generate_id <- function() {
    paste(sample(.candidates, 32, TRUE), collapse = "")
  }

  .id <- function() {
    id <- .generate_id()
    while (exists(id, envir = .ids, inherits = FALSE))
      id <- .generate_id()
    .ids[[id]] <- id
    id
  }

  # Public ----

  emit <- function(event, ...) {

    handlers <- .get_handlers(event)
    for (handler in rev(handlers$get())) {

      stopped <- FALSE
      status <- withCallingHandlers(
        tryCatch(handler(...), error = identity),
        stop_propagation = function(e) stopped <<- TRUE
      )

      if (stopped)
        return(invisible(NULL))
    }

    invisible(NULL)
  }

  on <- function(event, fn, scoped = TRUE, envir = parent.frame()) {
    handlers <- .get_handlers(event)
    id <- .id()
    handlers$insert(id, fn)
    if (scoped)
      defer(off(event, id), envir = envir)
    id
  }

  off <- function(event, handler_id) {
    handlers <- .get_handlers(event)
    handlers$remove(handler_id)
  }

  list(emit = emit, on = on, off = off)
}

#' Events
#'
#' Simple tools for emitting and handling events. \code{on} and \code{off} are
#' used to register and unregister event handlers, and \code{emit} is used to
#' emit an event for registered handlers to handle.
#'
#' These functions interact with a global handler registration -- if you need to
#' define separate handler registrations, you can use
#' \code{\link{create_handler_registration}} to generate your own, and call the
#' relevant \code{on}, \code{off} and \code{emit} events on that object.
#'
#' @param event The name of a event, as a string.
#' @param fn A function to be executed in response to a emit.
#' @param id A handler id, as returned by \code{on}. Use this to
#'  remove a registered handler with \code{off} later.
#' @param scoped Boolean; should this event handler be scoped
#'  to the current function body?
#' @param ... Arguments to be passed to registered event handlers.
#'
#' @examples
#' # use event system in increment a counter
#' counter <- 0
#' foo <- function() {
#'   on("increment", function() { counter <<- counter + 1})
#'   emit("increment")
#' }
#'
#' # call once -- increment counter to 1
#' foo()
#' print(counter)
#'
#' # call again -- increment counter to 2
#' foo()
#' print(counter)
#'
#' # fire event with no listener -- no increment
#' emit("increment")
#' print(counter)
#'
#' # generate a new handler registration, and fire an
#' # event on that object
#' registration <- create_handler_registration()
#' registration$emit("increment")
#'
#' # counter is still 2
#' print(counter)
#'
#' @rdname events
#' @name events
#' @export
on <- function(event, fn, scoped = TRUE) {
  .__HANDLERS__.$on(event, fn, scoped, parent.frame())
}

#' @rdname events
#' @name events
#' @export
off <- function(event, id) {
  .__HANDLERS__.$off(event, id)
}

#' @rdname events
#' @name events
#' @export
emit <- function(event, ...) {
  .__HANDLERS__.$emit(event, ...)
}

signal_condition <- function(class = NULL, data = NULL) {
  condition <- structure(data, class = c(class, "condition"))
  signalCondition(condition)
}

#' @rdname events
#' @name events
#' @export
stop_propagation <- function() {
  signal_condition("stop_propagation")
}
