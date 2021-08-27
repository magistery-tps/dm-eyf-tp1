options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('../lib/import.R')
import('./common.R')
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
raw_dev_set <- loadcsv("../dataset/paquete_premium_202009.csv")
test_set    <- loadcsv("../dataset/paquete_premium_202011.csv")

dev_set <- preprocessing(raw_dev_set, excludes = excluded_columns)
show_groups(dev_set)
ncol(dev_set)
ncol(test_set)


# Split train-val...
c(train_set, val_set) %<-% train_test_split(dev_set, train_size=.9, shuffle=TRUE)
stratify_metrics(train_set, val_set)

params <- xgb_default_params()
params$max_depth   <- 2
nrounds            <- 20
params$alpha       <- 15
params$eval_metric <- 'auc'
params$gamma       <- 0

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
k                = 10
beta             = 2
max_depth_values = seq(2, 4, 1)
nrounds_values   = seq(30, 40, 10)
alpha_values     = seq(0, 10, 5)
gamma_values     = seq(0, 10, 5)
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
fwrite(
  metrics,
  file='xgboost_cv_fold_metrics.csv',
  sep=","
)

fwrite(
  mean_by_fold_and_params(metrics), 
  file='xgboost_cv_mean_metrics.csv', 
  sep=","
)

mean_metrics <- fread(file='xgboost_cv_mean_metrics.csv', sep="," )

mean_metrics <- mean_metrics %>% mutate(
  train_gain     = currency(train_gain),
  val_gain       = currency(val_gain), 
  gain_diff      = currency(gain_diff)
) %>% arragen(gain_diff)


View(mean_metrics)

gplot_hist(mean_metrics$gain_diff, binwidth=0.05)

box_plot(mean_metrics$gain_diff)
box <- box_plot(mean_metrics$gain_diff)
box$stats

selected <- mean_metrics %>% filter(
  gain_diff >= (box$stats[3] - (box$stats[3]*0.05)),
  gain_diff <= (box$stats[3] + (box$stats[3]*0.05))
) %>% arrange(gain_diff)

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

xgb.plot.tree(model=dev_model, trees = 10)

# Save prediction...
setwd(this.path::this.dir())
save_result(test_set, test_pred)
