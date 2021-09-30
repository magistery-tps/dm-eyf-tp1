cat("\014")
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, lightgbm)
setwd(this.path::this.dir())
source('../../lib/import.R')
#
import('../src/common.R')
# ------------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Load & preprocess dataset
# -----------------------------------------------------------------------------
# Load dev y test
dataset_type <- 'enriched' # original'
raw_dev_set <- load_train_set(type=dataset_type)
test_set    <- load_test_set(type=dataset_type)
dev_set     <- preprocessing(raw_dev_set, excludes = excluded_columns)

colnames(dev_set)
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
  objective          = "binary",
  max_bin            = 15,
  min_data_in_leaf   = 4000,
  learning_rate      = 0.05,
  metric             = 'auc',
  first_metric_only  = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  seed               = 999983.0,
  force_row_wise     = TRUE
)

cv_result <- lgb.cv(
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

sum(test_pred)

# Save prediction...
save_model_result(
  result       = kaggle_df(test_set, test_pred),
  model_name   = 'light-gbm',
  hyper_params = params
)
# -----------------------------------------------------------------------------
#
#
#
#
