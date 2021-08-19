# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(foreach)
# ------------------------------------------------------------------------------
#
#
#

#
#
# Devuelve únicamente los features del dataset.
#
features <- function(df, target_col) {
  if(target_col %in% colnames(df)) {
    df %>% dplyr::select(-target_col)    
  } else {
    df
  }
}

#
# Devuelve únicamente la variable label.
#
label <- function(df, target_col) {
  df %>% 
    dplyr::select(target_col) %>% 
    dplyr::pull()
}

#
# Calcula la medio de una tabla de frecuencias
#
freq_table_mean <- function(freq_table) {
  sum(freq_table$value * freq_table$frequency) / sum(freq_table$frequency)
}


#
# Agrega el indice como una columna mas del dataframe
#
index_as_column <- function(df) {
  df <- cbind(index = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  df
}


#
# Itera las columna de un dataframe invoca el callback pasando el dataframe
# y el indice de la columna actual.
#
foreach_col <- function(df, callback) {
  foreach(col_index= 1:ncol(df), .combine = rbind) %dopar% {
    callback(df, col_index)
  }
}
