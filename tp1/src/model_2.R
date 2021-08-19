options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, data.table)
setwd(this.path::this.dir())
source('../lib/import.R')
import('../lib/common-lib.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
xgboost_predict <- function(model, features, threshold = 0.025) {
  predictions <- predict(model, as.matrix(features))
  data.frame(target = predictions) %>% 
    dplyr::mutate(target = as.numeric(target > threshold)) %>%
    pull()
}

#
# Genero el archivo para Kaggle
# URL: https://www.kaggle.com/c/uba-dmeyf2021-primera/
#
save_result <- function(test_set, test_pred, path="./K101_001.csv") {
  result <- test_set %>%
    dplyr::mutate(Predicted = test_pred) %>%
    dplyr::select(numero_de_cliente, Predicted)

  fwrite(result, file=path, sep="," )
}

preprocessing <- function(dev_set) {
  dev_set %>%
    dplyr::rename(target = clase_ternaria) %>%
    dplyr::mutate(target = as.numeric(as.factor(target))-1) %>%
    dplyr::mutate(target = ifelse(target==2, 0, ifelse(target==1, 1, 0)))
}

show_groups <- function(data) data %>% group_by(target) %>% tally()
feat        <- function(data) data %>% dplyr::select(-target)
target      <- function(data) data %>% dplyr::select(target) %>% pull()

xgboost_train <- function(data, max_depth=5, nround=70) {
  xgboost(
    data        = as.matrix(feat(data)),
    label       = target(data),
    max_depth   = max_depth,
    nround      = nround,
    nthread     = 24,
    verbose     = 1,
    eval_metric = 'logloss',
    objective   = "binary:logistic"
  )
}
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
# Cargamos dev y test
setwd(this.path::this.dir())
raw_dev_set <- loadcsv("../dataset/paquete_premium_202009.csv")
test_set  <- loadcsv("../dataset/paquete_premium_202011.csv")

dev_set <- preprocessing(raw_dev_set)
show_groups(dev_set)

# Split en train y val
c(train_set, val_set) %<-% train_test_split(dev_set, train_size=.9, shuffle=TRUE)
show_groups(train_set)
show_groups(val_set)

# model <- xgboost_train(train_set, max_depth=5, nround=70)
val_model <- xgboost_train(train_set, max_depth=2, nround=60)

val_pred <- xgboost_predict(val_model, feat(val_set))
val_real <- target(val_set)

plot_cm(val_pred, val_real)
plot_roc(val_pred, val_real)

dev_model <- xgboost_train(dev_set, max_depth=2, nround=60)
test_pred <- xgboost_predict(dev_model, test_set %>% dplyr::select(-clase_ternaria))
save_result(test_set, test_pred)
