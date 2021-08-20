options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, data.table, foreach)
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
# Generate Kaggle predictions file
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

xgboost_train <- function(
  data, 
  nthread   = 24, 
  max_depth = 5, 
  nround    = 70, 
  verbose   = 1,
  eta       = 0.3, 
  alpha     = 0
) {
  xgboost(
    data        = as.matrix(feat(data)),
    label       = target(data),
    max_depth   = max_depth,
    nround      = nround,
    eta         = eta,
    alpha       = alpha,
    verbose     = verbose,
    nthread     = nthread,
    eval_metric = 'logloss',
    objective   = "binary:logistic"
#    tree_method = 'gpu_hist',
#    predict     = 'gpu_predictor',
  )
}
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
# Load dev y test
setwd(this.path::this.dir())
raw_dev_set <- loadcsv("../dataset/paquete_premium_202009.csv")
test_set    <- loadcsv("../dataset/paquete_premium_202011.csv")

dev_set <- preprocessing(raw_dev_set)
show_groups(dev_set)

# Split train-val...
c(train_set, val_set) %<-% train_test_split(dev_set, train_size=.7, shuffle=TRUE)
show_groups(train_set)
show_groups(val_set)


# Train over train set and predict val set...
beta             <- 2
max_depth_values <- seq(1)
nrounds_values   <- seq(30, 40)
eta              <- 0.3
scores           <- data.frame()

for(nround in nrounds_values) {
  for(max_depth in max_depth_values) {
    val_model <- xgboost_train(train_set, max_depth=max_depth, nround=nround, verbose = 0, eta=eta)
    val_pred  <- xgboost_predict(val_model, feat(val_set))
    val_real  <- target(val_set)
    score     <- fbeta_score(val_pred, val_real, beta=beta, show = F)
    scores    <- rbind(scores, c(max_depth, nround, score)) 
    print(paste('max_depth:', max_depth, 'nround:', nround, 'score:', score))
  }
}
colnames(scores) <- c('Max Depth', 'N Round', 'F2Score')
scores %>% dplyr::arrange(dplyr::desc(F2Score))


plot_cm(val_pred, val_real)
aur(val_pred, val_real)
plot_roc(val_pred, val_real)

# xgb.plot.tree(model = val_model, trees = 1)
xgb.plot.tree(model = val_model, trees = 18)

# Train over dev set and predict test set...
dev_model <- xgboost_train(dev_set, max_depth=1, nround=34)
test_pred <- xgboost_predict(dev_model, test_set %>% dplyr::select(-clase_ternaria))

# Save prediction...
save_result(test_set, test_pred)
