#' Defer Evaluation of an Expression
#'
#' Similar to \code{\link{on.exit}()}, but allows one to attach
#' an expression to be evaluated when exitting any frame currently
#' on the stack.
#'
#' @param expr An expression to be evaluated.
#' @param envir Attach exit handlers to this environment.
#'   Typically, this should be either the current environment or
#'   a parent frame (accessed through \code{\link{parent.frame}()}).
#' @param priority Specify whether if this handler should be executed
#'   \code{"first"} or \code{"last"}, relative to any other registered
#'   handlers on this environment.
#'
#' @export
defer <- function(expr, envir = parent.frame(), priority = c("first", "last")) {

  priority <- match.arg(priority)
  front <- priority == "first"

  call <- substitute(
    evalq(expr, envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  add_handler(envir, call, front)
}
