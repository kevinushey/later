# TODO: Export this so objects can maintain their
# own set of handlers
Handlers <- R6::R6Class(

  "Handlers",

  public = list(

    initialize = function() {
      private$.handlers <- new.env(parent = emptyenv())
    },

    signal = function(signal, ...) {
      handlers <- private$handlers(signal)
      for (handler in objects(handlers)) {
        handler <- get(handler, envir = handlers)
        try(handler(...), silent = TRUE)
      }
    },

    on = function(signal, fn) {
      handlers <- private$handlers(signal)
      id <- digest::digest(fn)
      handlers[[id]] <- fn
      id
    },

    off = function(signal, id) {
      handlers <- private$handlers(signal)
      handlers[[id]] <- NULL
    }

  ),

  private = list(

    handlers = function(signal) {
      handlers <- private$.handlers
      if (!exists(signal, envir = handlers))
        assign(signal, new.env(parent = emptyenv()), envir = handlers)
      get(signal, envir = handlers)
    },

    .handlers = NULL
  )

)

.__HANDLERS__. <- Handlers$new()

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
  id <- .__HANDLERS__.$on(signal, fn)
  if (scoped)
    defer_parent(.__HANDLERS__.$off(signal, id))
  id
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
