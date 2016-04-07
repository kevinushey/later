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
  .handlers <- function(signal) {
    handlers <- .env
    if (!exists(signal, envir = handlers))
      assign(signal, new_list(), envir = handlers)
    get(signal, envir = handlers)
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

  signal <- function(signal, ...) {
    handlers <- .handlers(signal)
    for (handler in rev(handlers$get())) {
      status <- try(handler(...), silent = TRUE)
      if (identical(status, FALSE))
        return(invisible(NULL))
    }
    invisible(NULL)
  }

  on <- function(signal, fn, scoped = TRUE, envir = parent.frame()) {
    handlers <- .handlers(signal)
    id <- .id()
    handlers$insert(id, fn)
    if (scoped)
      defer(off(signal, id), envir = envir)
    id
  }

  off <- function(signal, id) {
    handlers <- .handlers(signal)
    handlers$remove(id)
  }

  list(signal = signal, on = on, off = off)
}

.__HANDLERS__. <- new_handlers()

#' Signals
#'
#' Simple tools for emitting signals (events).
#'
#' @param signal The name of a signal, as a string.
#' @param fn A function to be executed in response to a signal.
#' @param id A signal id, as returned by \code{on}.
#' @param scoped Boolean; should this signal handler be scoped
#'  to the current function body?
#' @param ... Arguments to be passed to registered signal handlers.
#'
#' @examples
#' # Show how 'on', 'signal' can be used to execute
#' # R functions as requested
#' counter <- 0
#' foo <- function() {
#'   on("increment", function() { counter <<- counter + 1})
#'   signal("increment")
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
#' # Fire signal with no listener -- no increment
#' signal("increment")
#' print(counter)
#'
#' @rdname signals
#' @name signals
#' @export
on <- function(signal, fn, scoped = TRUE) {
  .__HANDLERS__.$on(signal, fn, scoped, parent.frame())
}

#' @rdname signals
#' @name signals
#' @export
off <- function(signal, id) {
  .__HANDLERS__.$off(signal, id)
}

#' @rdname signals
#' @name signals
#' @export
signal <- function(signal, ...) {
  .__HANDLERS__.$signal(signal, ...)
}
