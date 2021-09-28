cat("\014")
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, lightgbm, yaml)
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
build_dataset <- function(
  dev_set, 
  test_set,
  excluded_columns = c(
    'numero_de_cliente', 
    'foto_mes',
    'ccajas_transacciones', 
    'Master_mpagominimo',
    'clase_ternaria'
  )
) {
  dev_set <- dev_set %>% 
    dplyr::select(-excluded_columns) %>% 
    mutate(target=1)
  
  test_set <- test_set %>% 
    dplyr::select(-excluded_columns) %>% 
    mutate(target=0)
  
  # setdiff(colnames(dev_set), colnames(test_set))
  
  union_all(dev_set, test_set)
}


model_cv <- function(
  dataset,
  params  = list(objective='binary', metric='auc'),
  nrounds = 10,
  nthread = 24,
  nfold   = 10
) {
  train_set <- lgb.Dataset(
    data  = data.matrix(feat(dataset)),
    label = target(dataset)
  )
  lgb.cv(
    nfold   = nfold,
    data    = train_set, 
    params  = params,
    nrounds = nrounds, 
    nthread = nthread
  )
}

best_booster <- function(cv_result) cv_result$boosters[cv_result$best_iter][[1]]$booster

train_model <- function(dataset) {
  cv_result    <- model_cv(dataset)
  best_booster <- best_booster(cv_result)
  tree         <- lgb.importance(best_booster)

  list(
    model    = best_booster,
    tree     = tree,
    features = tree$Feature,
    score    = cv_result$best_score
  )
}

data_drift_report <- function(dataset, columns=NULL) {
  result <- train_model(dataset)
  
  if(is.null(columns)) {
    columns <- result$features
  }
  
  report <- data.frame(feature=character(), auc=numeric())
  for(feature in columns) {
    new_features <- setdiff(colnames(dataset), list(feature))

    new_dataset <- dataset %>% dplyr::select(c(new_features, target))
    
    result <- train_model(new_dataset)
    
    report <- report %>% add_row(feature=feature, auc=result$score)
  }
  report %>% 
    mutate(auc_norm = norm(auc)) %>%
    arrange(desc(auc_norm))
}

norm <- function(m) (m - min(m))/(max(m)-min(m))
#
#
#
#
# -----------------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------------
setwd(this.path::this.dir())
dev_set  <- load_train_set() 
test_set <- load_test_set()
dataset  <- build_dataset(dev_set, test_set)
dataset %>% group_by(target) %>%tally()


cv_result <- train_model(dataset)
lgb.plot.importance(cv_result$tree)

report <- data_drift_report(dataset)
report
fwrite(report, file=paste('../../reports/data_drift_report_2.csv', sep=''), sep=",")


report<- data_drift_report(
  dataset, 
  setdiff(colnames(dataset), list(target))
)
report
fwrite(report, file=paste('../../reports/data_drift_report_3.csv', sep=''), sep=",")
# -----------------------------------------------------------------------------
#
#
#
#

