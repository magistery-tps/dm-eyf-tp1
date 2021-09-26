cat("\014")
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, dplyr, data.table, rpart)
setwd(this.path::this.dir())
source('../lib/import.R')
#
import('../src/common.R')
# ------------------------------------------------------------------------------
#
#

# Cargo los datos de 202009 que es donde voy a ENTRENAR el modelo...
train_set <- load_train_set()
test_set  <- load_test_set()


# Genero el modelo...
params <- list(
  xval      = 0,
  cp        = -0.3, 
  minsplit  = 80,
  minbucket = 1,
  maxdepth  = 8
)
model  <- rpart(
  "clase_ternaria ~ .",
  data      = train_set,
  xval      = params$xval,
  cp        = params$cp, 
  minsplit  = params$minsplit,
  minbucket = params$minbucket,
  maxdepth  = params$maxdepth
)


# Aplico al modelo  a los datos de 202011
test_predictions  <- predict(model, test_set , type = "prob") #aplico el modelo

# test_predictions es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades. 

result <- test_set %>%
  mutate(prob_baja2 = test_predictions[, "BAJA+2"]) %>%
  mutate(Predicted  = as.numeric(prob_baja2 > 0.025)) %>% 
  dplyr::select(numero_de_cliente, Predicted)
  
# Genero el archivo para Kaggle
save_model_result(
  result       = result,
  model_name   = 'rpart',
  hyper_params = params
)
