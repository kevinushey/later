## Handlers used for 'defer' calls.
## Attached as a list of expressions for
## the 'handlers' attribute on the
## environment, with 'on.exit' called to
## ensure those handlers get executed on
## exit.

get_handlers <- function(envir) {
  result <- attr(envir, "handlers")
  if (!is.list(result)) list() else result
}

set_handlers <- function(envir, handlers) {
  attr(envir, "handlers") <- handlers
  if (is.null(attr(envir, "has_handlers"))) {

    attr(envir, "has_handlers") <- TRUE

    call <- substitute(
      f(x),
      list(f = execute_handlers, x = envir)
    )

    do.call(
      base::on.exit,
      list(substitute(call), TRUE),
      envir = envir
    )
  }
}

execute <- function(object) {
  if (is.language(object))
    eval(object)
  else if (is.function(object))
    object()
}

execute_handlers <- function(envir) {
  handlers <- get_handlers(envir)
  for (handler in handlers)
    execute(handler)
}

add_handler <- function(envir, handler, front) {
  handlers <- if (front)
    c(handler, get_handlers(envir))
  else
    c(get_handlers(envir), handler)

  set_handlers(envir, handlers)
}
