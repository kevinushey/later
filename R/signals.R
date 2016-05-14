new_list <- function() {
  .data <- list()

  insert = function(key, val) {
    .data[[key]] <<- val
  }

  remove <- function(key) {
    .data[[key]] <<- NULL
  }

  contains <- function(key) {
    key %in% names(.data)
  }

  get <- function() {
    .data
  }

  list(
    insert = insert,
    remove = remove,
    contains = contains,
    get = get
  )

}

#' Create a New Handler Registration
#'
#' Create a new handler registration. Objects generated from this function can
#' implement the interface described in \code{\link{events}} -- handlers can be
#' attached and detached with \code{on} / \code{off}. and events can be emitted
#' with \code{emit}.
#'
#' @export
create_handler_registration <- function() {

  # Private ----

  ## Handlers
  .env <- new.env(parent = emptyenv())
  .get_handlers <- function(event) {
    handlers <- .env
    if (!exists(event, envir = handlers))
      assign(event, new_list(), envir = handlers)
    get(event, envir = handlers)
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
#' Simple tools for emitting and handling events.
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
#' # Show how 'on', 'emit' can be used to execute
#' # R functions as requested. Note that the handler registered
#' # by 'on()' below is active only within that function's body
#' # -- it is 'destroyed' once 'foo()' finishes execution.
#' counter <- 0
#' foo <- function() {
#'   on("increment", function() { counter <<- counter + 1})
#'   emit("increment")
#' }
#'
#' # Call once -- increment counter to 1
#' foo()
#' print(counter)
#'
#' # Call again -- increment counter to 2
#' foo()
#' print(counter)
#'
#' # Fire emit with no listener -- no increment
#' emit("increment")
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

#' @rdname events
#' @name events
#' @export
stop_propagation <- function() {
  condition <- structure(NULL, class = c("stop_propagation", "condition"))
  signalCondition(condition)
}
