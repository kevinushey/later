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

new_handlers <- function() {

  # Private ----

  ## Handlers
  .env <- new.env(parent = emptyenv())
  .handlers <- function(emit) {
    handlers <- .env
    if (!exists(emit, envir = handlers))
      assign(emit, new_list(), envir = handlers)
    get(emit, envir = handlers)
  }

  ## IDs
  .candidates <- c(letters, LETTERS, 0:9)
  .generate_id <- function() {
    paste(sample(.candidates, 32, TRUE), collapse = "")
  }
  .ids <- new.env(parent = emptyenv())
  .id <- function() {
    id <- .generate_id()
    while (!is.null(.ids[[id]]))
      id <- .generate_id()
    .ids[[id]] <- id
    id
  }

  # Public ----

  emit <- function(emit, ...) {

    handlers <- .handlers(emit)
    for (handler in rev(handlers$get())) {

      stopped <- FALSE
      status <- withCallingHandlers(
        try(handler(...), silent = TRUE),
        stop_propagation = function(e) stopped <<- TRUE
      )

      if (stopped)
        return(invisible(NULL))
    }

    invisible(NULL)
  }

  on <- function(emit, fn, scoped = TRUE, envir = parent.frame()) {
    handlers <- .handlers(emit)
    id <- .id()
    handlers$insert(id, fn)
    if (scoped)
      defer(off(emit, id), envir = envir)
    id
  }

  off <- function(emit, id) {
    handlers <- .handlers(emit)
    handlers$remove(id)
  }

  list(emit = emit, on = on, off = off)
}

.__HANDLERS__. <- new_handlers()

#' events
#'
#' Simple tools for emitting events (events).
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
#' # R functions as requested
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
