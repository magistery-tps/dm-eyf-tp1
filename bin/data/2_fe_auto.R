cat("\014")
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, dplyr, xgboost, Matrix)
setwd(this.path::this.dir())
source('../../lib/import.R')
#
import('../src/common.R')
# ------------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Functions
# -----------------------------------------------------------------------------
discover_features <- function(
  train_set,
  test_set,
  params  = list(max_depth=10, eta=1, objective='binary:logistic', nthread=24),
  nrounds = 10
) {
  data  <- xgb.DMatrix(data=data.matrix(train_set), label=target(train_set), missing=NA)
  model <- xgb.train(params, data, nrounds)

  new_train_set <- xgb_create_features(model, train_set)
  new_test_set  <- xgb_create_features(model, test_set)
  list(new_train_set, new_test_set)
}

original_train_set <- load_train_set()
original_test_set  <- load_test_set() 
colnames(original_train_set)
colnames(original_test_set)


train_set <- preprocessing(original_train_set, excludes = excluded_columns) 
test_set  <- original_test_set %>% 
  dplyr::select(-c(excluded_columns)) %>% 
  dplyr::rename(target='clase_ternaria')


c(new_train_features, new_test_set_features) %<-% discover_features(
  train_set, 
  test_set,
  nrounds = 10
)

final_train_set       <- cbind(original_train_set, new_train_features)
colnames(final_train_set)

final_test_set        <- cbind(original_test_set, new_test_set_features)
colnames(final_train_set)


fwrite(final_train_set, file="../../dataset/enriched/paquete_premium_202009.csv", sep= "," )
fwrite(final_test_set,  file="../../dataset/enriched/paquete_premium_202011.csv", sep= "," )
# -----------------------------------------------------------------------------
#
#
#
#

