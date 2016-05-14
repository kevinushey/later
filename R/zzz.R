.__HANDLERS__. <- NULL

.onLoad <- function(lib, pkg) {
  .__HANDLERS__. <<- create_handler_registration()
  .__HANDLERS__.
}
