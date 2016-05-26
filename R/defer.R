#' Defer Evaluation of an Expression
#'
#' Similar to \code{\link{on.exit}()}, but allows one to attach
#' an expression to be evaluated when exitting any frame currently
#' on the stack. This provides a nice mechanism for scoping side
#' effects for the duration of a function's execution.
#'
#' @param expr An expression to be evaluated.
#' @param envir Attach exit handlers to this environment.
#'   Typically, this should be either the current environment or
#'   a parent frame (accessed through \code{\link{parent.frame}()}).
#' @param priority Specify whether this handler should be executed
#'   \code{"first"} or \code{"last"}, relative to any other registered
#'   handlers on this environment.
#'
#' @details
#'
#' \code{defer} works by attaching handlers to the requested environment (as an
#' attribute called \code{"handlers"}), and registering an exit handler that
#' executes the registered handler when the function associated with the
#' requested environment finishes execution.
#'
#' @family scope-related functions
#' @export
#' @examples
#' # define a 'scope' function that creates a file, and
#' # removes it when the parent function has finished executing
#' scope_file <- function(path) {
#'   file.create(path)
#'   defer_parent(unlink(path))
#' }
#'
#' # create tempfile path
#' path <- tempfile()
#'
#' # use 'scope_file' in a function
#' local({
#'   scope_file(path)
#'   stopifnot(file.exists(path))
#' })
#'
#' # file is deleted as we leave 'local' scope
#' stopifnot(!file.exists(path))
#'
#' # investigate how 'defer' modifies the
#' # executing function's environment
#' local({
#'   scope_file(path)
#'   print(attributes(environment()))
#' })
defer <- function(expr, envir = parent.frame(), priority = c("first", "last")) {

  if (identical(envir, .GlobalEnv))
    stop("attempt to defer event on global environment")

  priority <- match.arg(priority)
  front <- priority == "first"

  call <- substitute(
    evalq(tryCatch(expr, error = identity), envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  invisible(add_handler(envir, call, front))
}

#' @rdname defer
#' @export
defer_parent <- function(expr, priority = c("first", "last")) {
  eval(substitute(
    defer(expr, envir, priority),
    list(expr = substitute(expr), envir = parent.frame(2), priority = priority)
  ), envir = parent.frame())
}


## Handlers used for 'defer' calls. Attached as a list of expressions for the
## 'handlers' attribute on the environment, with 'on.exit' called to ensure
## those handlers get executed on exit.

get_handlers <- function(envir) {
  result <- attr(envir, "handlers")
  if (!is.list(result)) list() else result
}

set_handlers <- function(envir, handlers) {
  attr(envir, "handlers") <- handlers
  if (is.null(attr(envir, "has_handlers"))) {
    attr(envir, "has_handlers") <- TRUE
    call <- make_call(execute_handlers, envir)
    do.call(base::on.exit, list(substitute(call), TRUE), envir = envir)
  }
}

execute_handlers <- function(envir) {
  handlers <- get_handlers(envir)
  for (handler in handlers)
    eval(handler)
}

add_handler <- function(envir, handler, front) {
  handlers <- if (front)
    c(handler, get_handlers(envir))
  else
    c(get_handlers(envir), handler)

  set_handlers(envir, handlers)
  handler
}
