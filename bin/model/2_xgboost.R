cat("\014")
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('../../lib/import.R')
#
import('../src/common.R')
import('../src/xgboost.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
# Load dev y test
setwd(this.path::this.dir())
raw_dev_set <- load_train_set()
test_set    <- load_test_set()

dev_set <- preprocessing(raw_dev_set, excludes = excluded_columns)
show_groups(dev_set)
ncol(dev_set)
ncol(test_set)


# Split train-val...
c(train_set, val_set) %<-% train_test_split(dev_set, train_size=.9, shuffle=TRUE)
stratify_metrics(train_set, val_set)

params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 60
params$eval_metric <- 'auc'
params$verbose     <- TRUE

metrics <- train_model(train_set, val_set, params = params, nrounds = nrounds)
data.frame(metrics)
#
#
#
# ------------------------------------------------------------------------------
# Cross validation and grid search:
# ------------------------------------------------------------------------------
#
# Hiperparametros:
k                <- 5
beta             <- 2
eta_values       <- c(0.3, 0.5, 0.7, 1)
max_depth_values <- seq(2, 3, 1)
nrounds_values   <- seq(20, 40, 10)
alpha_values     <- seq(0, 10, 5)
gamma_values     <- seq(0, 10, 5)
#
metrics <- cv_callback(
  dev_set, 
  grid_search_fn(
    beta,
    eta_values,
    max_depth_values,
    nrounds_values,
    alpha_values,
    gamma_values
  ),
  k=k
)
#
fwrite(
  metrics,
  file='xgboost_cv_fold_metrics2.csv',
  sep=","
)

fwrite(
  mean_by_fold_and_params(metrics), 
  file='xgboost_cv_mean_metrics2.csv', 
  sep=","
)

mean_metrics <- fread(file='xgboost_cv_mean_metrics.csv', sep="," )

mean_metrics <- mean_metrics %>% dplyr::mutate(
  train_gain     = currency(train_gain),
  val_gain       = currency(val_gain), 
  gain_diff      = currency(gain_diff)
) %>% dplyr::arrange(gain_diff)

View(mean_metrics)

gplot_hist(mean_metrics$f2_score_diff, binwidth=0.01)

box_plot(mean_metrics$f2_score_diff)
box <- box_plot(mean_metrics$f2_score_diff)
box$stats

selected <- mean_metrics %>% filter(
  f2_score_diff >= (box$stats[3] - (box$stats[3]*0.05)),
  f2_score_diff <= (box$stats[3] + (box$stats[3]*0.05))
) %>% arrange(f2_score_diff)

nrow(selected)
# View(selected)
selected[1]
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
nrounds            <- 20 #30
params$alpha       <- 15
params$gamma       <- 0
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Kaggle Score: 11.96129 *
params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 70 #30
params$alpha       <- 5
params$gamma       <- 1
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
# Kaggle Score: 10.98223 (AUC Train/Val con el menor overfitting, hasta el momento).
params <- xgb_default_params()
params$max_depth   <- 3
nrounds            <- 40
params$alpha       <- 10
params$gamma       <- 15
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)

# Kaggle Score: 11.39052
params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 60
params$alpha       <- 5
params$gamma       <- 5
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)


# Clase baja2+1 vs continua:
params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 40
params$alpha       <- 10
params$gamma       <- 0
params$eta         <- 0.3
params$eval_metric <- 'auc'
nfold              <- 5
params$verbose     <- T

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)


# Clase baja2+1 vs continua: 11.03639
params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 30
params$alpha       <- 5
params$gamma       <- 10
params$eta         <- 0.5
params$eval_metric <- 'auc'
nfold              <- 5
params$verbose     <- T

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)


#
#
#
# Kaggle Score: 
params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 7
params$alpha       <- 9
params$gamma       <- 14
params$eval_metric <- 'auc'
nfold              <- 10

model <- xgboost_cv(dev_set, params, nfold=nfold, nrounds=nrounds)
plot_xgboost_cv_train_vs_val(model)
#
#
#
#
# Train over dev set and predict test set...
dev_model <- xgboost_train(dev_set, params, nrounds)
test_pred <- xgboost_predict(
  dev_model, 
  test_set %>% dplyr::select(-c(excluded_columns, clase_ternaria))
)

xgb.plot.tree(model=dev_model, trees = nrounds-1)

# Save prediction...
save_model_result(
  result       = kaggle_df(test_set, test_pred),
  model_name   = 'xgboost',
  hyper_params = params
)
  
