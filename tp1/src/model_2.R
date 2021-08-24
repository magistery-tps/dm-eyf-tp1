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
# Variables
# ------------------------------------------------------------------------------
metric_cols <- c('fold', 'max_depth', 'nrounds', 'alpha', 'gamma', 'score')
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
create_df <- function(columns) {
  df <- data.frame(matrix(ncol = length(columns), nrow = 0))
  colnames(df) <- columns
  df
}

add <- function(df, row, columns) {
  df <- rbind(df, row)
  colnames(df) <- columns
  df
}

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

train_and_metrics <- function(
  train_set,
  val_set,
  beta    = 2,
  params  = xgb_default_params(), 
  nrounds = 10
) {
  print(params)
  model <- xgboost_train(train_set, params, nrounds)
  val_pred  <- xgboost_predict(model, feat(val_set))
  val_real  <- target(val_set)
  score     <- fbeta_score(val_pred, val_real, beta=beta, show = F)
  list(score, model)
}

grid_search_fn <- function(
  beta, 
  max_depth_values, 
  nrounds_values,
  alpha_values,
  gamma_values
) {
  function(fold, train_set, val_set) {
    metrics <-create_df(metric_cols)
    for(nrounds in nrounds_values) {
      for(max_depth in max_depth_values) {
        for(alpha in alpha_values) {
          for(gamma in gamma_values) {

            params <- xgb_default_params()
            params$max_depth <- max_depth
            params$alpha     <- alpha       
            params$gamma     <- gamma

            c(score, model) %<-% train_and_metrics(
              train_set, 
              val_set, 
              beta, 
              params, 
              nrounds
            )

            curr_metrics <- c(fold, max_depth, nrounds, alpha, gamma, score)
            metrics      <- add(metrics, curr_metrics, metric_cols)

            print(curr_metrics)
          }
        }
      }
    }
    metrics
    print(metrics)
  }
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
# c(train_set, val_set) %<-% train_test_split(dev_set, train_size=.7, shuffle=TRUE)
# show_groups(train_set)
# show_groups(val_set)
# c(score, model) %<-% train_and_metrics(train_set, val_set)
# score
#
#
#
# ------------------------------------------------------------------------------
# Cross validation and grid search:
# ------------------------------------------------------------------------------
#
# Hiperparametros:
k                = 10
beta             = 2
max_depth_values = seq(2, 6, 2)
nrounds_values   = seq(10, 40, 10)
alpha_values     = seq(0, 20, 5)
gamma_values     = seq(0, 20, 5)
#
metrics <- cv_callback(
  dev_set, 
  grid_search_fn(
    beta,
    max_depth_values,
    nrounds_values,
    alpha_values,
    gamma_values
  ),
  k=k
)
#
# Save metrics:
last_metrics <- metrics %>%
  group_by(across(all_of(metric_cols[2:5]))) %>% 
  summarise_at(vars(score), list(cv_mean_f2_score = mean)) %>%
  arrange(desc(cv_mean_f2_score))

View(last_metrics)
fwrite(last_metrics, file='xgboost_cv_metrics.csv', sep="," )
#
#
#
# ------------------------------------------------------------------------------
# Test models:
# ------------------------------------------------------------------------------
#
# Kaggle Score: 9.36
params <- xgb_default_params()
params$max_depth   <- 1
nrounds            <- 34
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Kaggle Score: 8.18
params <- xgb_default_params()
params$max_depth   <- 4
nrounds            <- 22
params$alpha       <- 0
params$gamma       <- 5
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Kaggle Score: 11.01972
params <- xgb_default_params()
params$max_depth   <- 4
nrounds            <- 22 # 40
params$alpha       <- 5
params$gamma       <- 0.1
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Kaggle Score: 11.54467
params <- xgb_default_params()
params$max_depth   <- 6
nrounds            <- 22 #30
params$alpha       <- 15
params$gamma       <- 0
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Kaggle Score: 10.71559
params <- xgb_default_params()
params$max_depth   <- 6
nrounds            <- 18 #30
params$alpha       <- 15
params$gamma       <- 0
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Train over dev set and predict test set...
dev_model <- xgboost_train(dev_set, params, nround)
test_pred <- xgboost_predict(dev_model, test_set %>% dplyr::select(-clase_ternaria))
xgb.plot.tree(model=dev_model, trees = nround-1)
# Save prediction...
save_result(test_set, test_pred)
