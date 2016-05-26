make_call <- function(...) {
  as.call(list(...))
}

ensure_all_named <- function(object, msg) {

  if (missing(msg))
    msg <- "all objects must be named"

  if (is.null(names(object)))
    stop(msg, call. = FALSE)

  if (!all(nzchar(names(object))))
    stop(msg, call. = FALSE)

  TRUE
}

new_list <- function() {

  # Private ----
  .data <- list()

  # Public ----
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
