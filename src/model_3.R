# https://www.kaggle.com/andrewmvd/lightgbm-in-r
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, lightgbm)
setwd(this.path::this.dir())
source('../lib/import.R')
import('./common.R')
# ------------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Load & preprocess dataset
# -----------------------------------------------------------------------------
# Load dev y test
setwd(this.path::this.dir())
raw_dev_set <- loadcsv("../dataset/paquete_premium_202009.csv")
test_set    <- loadcsv("../dataset/paquete_premium_202011.csv")
dev_set     <- preprocessing(raw_dev_set, excludes = excluded_columns)
# show_groups(dev_set)
# -----------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Make a CV over development dataset
# -----------------------------------------------------------------------------
train_set = lgb.Dataset(
  data  = data.matrix(feat(dev_set)), 
  label = target(dev_set)
)

params = list(
  objective        = "binary",
  max_bin          = 15,
  min_data_in_leaf = 4000,
  learning_rate    = 0.05
)

cv_result <- lgb.cv(
#  device          = "gpu",
#  gpu_platform_id = 0,
#  gpu_device_id   = 0,
#  gpu_use_dp      = TRUE,
  params          = params,
  nthread         = 24,
  nrounds         = 100,
  nfold           = 10,
  stratified      = TRUE,
  data            = train_set
)
cv_result$best_score
# -----------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Train model with all development examples
# -----------------------------------------------------------------------------
model <- lightgbm(
  train_set, 
  params=params,
  nrounds = 100,
  nthread = 24
)
# -----------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Predict over test_set
# -----------------------------------------------------------------------------
test_features <- test_set %>% 
  dplyr::select(-c(excluded_columns, 'clase_ternaria'))

test_pred <- light_gbm_predict(
  model,
  features  = data.matrix(test_features), 
  threshold = 0.031
)

# Save prediction...
save_result(test_set, test_pred, model_name='light-gbm', params=params)
# -----------------------------------------------------------------------------
#
#
#
#

