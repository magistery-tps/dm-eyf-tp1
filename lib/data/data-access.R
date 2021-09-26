# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(mongolite)
setwd(this.path::this.dir())
source('../import.R')
#
import('./plot/init.R')
# ------------------------------------------------------------------------------
#
#
#
#

#
# Devuelve una colección de una base de datos mongodb.
#
get_collection <- function(name, db="spotify") { 
  mongo(collection=name, db = db)
}

#
# Devuelve una proyección de colección de una base de datos mongodb.
#
get_collection_fields <- function(collection_name, field_name) {
  collection <- get_collection(collection_name)
  projection <- paste('{ "', field_name, '": 1 }', sep='')
  collection$find(fields = projection)
}

#
# Devuelve una tabla de frecuencias calculada en el motor de base de 
# datos mongodb.
#
get_freq_table <- function(collection_name) {
  hist_collection <- get_collection(collection_name)

  hist_collection$find(
    query = '{}', 
    fields = '{"_id": false, "frequency": true, "value": true}'
  )
}

#
# Gráfica de un histograma a partir de una tabla de frecuencias procesada en
# el motor mongodb.
#
plot_hist_collection <- function(collection_name, name, binwidth=0.5) {
  gplot_hist_from_freq_table(
    get_freq_table(collection_name),
    name=name,
    binwidth=binwidth
  )
}