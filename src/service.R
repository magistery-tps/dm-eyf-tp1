library(pacman)
p_load(plumber)

#* @apiTitle Model API

#* Ejecuta un modelo que requiere una entrada string.
#* @param input una entrada.
#* @get /model/predict
function(input = "") {
  list(
    output = paste("La entrada es:", input)
  )
}

# plumber::plumb("service.R")$run(port = 3000)
