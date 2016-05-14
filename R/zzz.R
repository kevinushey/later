.onLoad <- function(lib, pkg) {
  ns <- asNamespace(pkg)
  assign(".__HANDLERS__.", create_handler_registration(), envir = ns)
}
