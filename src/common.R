options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, yaml)
setwd(this.path::this.dir())
source('../lib/import.R')
import('./init.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Variables
# ------------------------------------------------------------------------------
excluded_columns = c(
  'numero_de_cliente', 
  'foto_mes',
  'ccajas_transacciones', 
  'Master_mpagominimo'
)
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
load_train_set <- function(type='original') {
  loadcsv(paste('../../dataset/', type , '/paquete_premium_202009.csv', sep=''))
}
load_test_set <- function(type='original') {
  loadcsv(paste('../../dataset/', type, '/paquete_premium_202011.csv', sep=''))
}

build_gain_filename_fn <- function(gain) {
  function(path, model_name, params) {
    paste(
      path,
      '/', 
      strftime(Sys.time(), format="%Y-%m-%d_%H-%M-%S"),
      '_',
      model_name,
      '_gain_',
      gain,
      sep=''
    )
  }  
}

kaggle_df <-function(test_set, test_pred) {
  test_set %>%
    dplyr::mutate(Predicted = test_pred) %>%
    dplyr::select(numero_de_cliente, Predicted)
}

preprocessing <- function(dev_set, excludes=c()) {
  dev_set %>%
    dplyr::rename(target = clase_ternaria) %>%
    dplyr::mutate(target = as.numeric(as.factor(target))-1) %>%
    dplyr::mutate(target = ifelse(target==2, 0, 1)) %>%
    dplyr::select(-excludes)
}

show_groups <- function(data) data %>% group_by(target) %>% tally()