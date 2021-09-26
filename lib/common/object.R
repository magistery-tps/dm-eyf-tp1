#
# Crea una instancia de un objeto.
#
new_instance <- function(class_name, ...) {
  attributes <- list(...)
  class(attributes) <- class_name
  attributes
}