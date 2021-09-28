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
# Prepare data
# -----------------------------------------------------------------------------
excluded_columns = c(
  'numero_de_cliente', 
  'foto_mes',
  'ccajas_transacciones', 
  'Master_mpagominimo',
  'clase_ternaria'
)

setwd(this.path::this.dir())
dev_set  <- load_train_set() %>% 
  dplyr::select(-excluded_columns) %>% 
  mutate(target=1)

test_set <- load_test_set() %>% 
  dplyr::select(-excluded_columns) %>% 
  mutate(target=0)

setdiff(colnames(dev_set), colnames(test_set))

dataset <- union_all(dev_set, test_set)
# -----------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Make a CV over development dataset
# -----------------------------------------------------------------------------
train_set = lgb.Dataset(
  data  = data.matrix(feat(dataset)), 
  label = target(dataset)
)

params = list(
  objective = "binary",
  metric    = 'auc'
)

cv_result <- lgb.cv(
  params          = params,
  nthread         = 24,
  nrounds         = 20,
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
  params  = params,
  nrounds = 20,
  nthread = 24
)
# -----------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Data Drift: Cuando mas importante es la variable mayor es el grado de 
#             data drift.
# -----------------------------------------------------------------------------
tree <- lgb.importance(model)
lgb.plot.importance(tree)

important_features <- data.frame(feature=tree$Feature)
View(important_features)
# -----------------------------------------------------------------------------
#
#
#
#