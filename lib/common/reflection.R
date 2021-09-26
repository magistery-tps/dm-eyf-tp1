#
# Crea una variable en tiempo de ejecución en el scope global
#
set_var_value <- function(name, value) {
  eval(
    parse(text =paste("{", name, "<-", value, "}", sep = "")), 
    envir=.GlobalEnv
  )
}

#
# Lee una variable en tiempo de ejecución en el scope global
#
get_var_value <- function(name) eval(parse(text = name), envir=.GlobalEnv)