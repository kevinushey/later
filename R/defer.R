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
#' (function() {
#'   scope_file(path)
#'   stopifnot(file.exists(path))
#' })()
#'
#' # file is deleted as we leave 'local' scope
#' stopifnot(!file.exists(path))
defer <- function(expr, envir = parent.frame(), priority = c("first", "last")) {

  if (identical(envir, .GlobalEnv))
    stop("attempt to defer event on global env")

  priority <- match.arg(priority)
  front <- priority == "first"

  call <- substitute(
    evalq(tryCatch(expr, error = identity), envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  add_handler(envir, call, front)
}

#' @rdname defer
#' @export
defer_parent <- function(expr, priority = c("first", "last")) {
  defer(expr, parent.frame(2), priority)
}
