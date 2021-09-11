options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, lightgbm, Matrix)
p_load_gh("krlmlr/ulimit")
setwd(this.path::this.dir())
source('../lib/import.R')
import('./common.R')
------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
ulimit::memory_limit(30000)

# https://www.kaggle.com/andrewmvd/lightgbm-in-r

# Load dev y test
setwd(this.path::this.dir())
raw_dev_set <- loadcsv("../dataset/paquete_premium_202009.csv")

dev_set <- preprocessing(raw_dev_set, excludes = excluded_columns)
show_groups(dev_set)

dev_features <- Matrix(as.matrix(feat(dev_set)),   sparse=TRUE)
dev_targets  <- Matrix(as.matrix(target(dev_set)), sparse=TRUE)
ds           <- lgb.Dataset(data=dev_features, label=dev_targets)

params = list(
  objective               = "binary",
  metric                  = "auc",
  max_depth               = 3,
  num_leaves              = 6, # 2^(max_depth)
  learning_rate           = 0.01,
  min_sum_hessian_in_leaf = 1,
  feature_fraction        = 0.7,
  bagging_fraction        = 1,
  bagging_freq            = 10,
  min_data                = 100,
  max_bin                 = 50,
  lambda_l1               = 8,
  lambda_l2               = 1.3,
  min_data_in_bin         = 10,
  min_gain_to_split       = 10,
  min_data_in_leaf        = 30,
  is_unbalance            = TRUE
)
  

dev_cv_model <- lgb.cv(
  params          = params,
#  device          = "gpu",
#  gpu_platform_id = 0,
#  gpu_device_id   = 0,
#  gpu_use_dp      = TRUE,
  nthread         = 24,
  data            = ds,
  nrounds         = 300,
  nfold           = 10,
  stratified      = TRUE
)
dev_cv_model$best_score

Cstack_info()

test_set      <- loadcsv("../dataset/paquete_premium_202011.csv")

test_features <- test_set %>% dplyr::select(-c(excluded_columns, clase_ternaria))

dtrain = dev_features <- model.matrix(~.-1, data=test_features) 

test_pred <- light_gbm_predict(dev_cv_model, dtrain)


# Save prediction...
save_result(test_set, test_pred)


