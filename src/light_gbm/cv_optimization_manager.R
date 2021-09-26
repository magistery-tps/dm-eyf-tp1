# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
p_load(this.path, lightgbm)
setwd(this.path::this.dir())
source('../../lib/import.R')
import('../../lib/object.R')
# ------------------------------------------------------------------------------
#
#
#
#
CVOptimizationContext <- function(optimization_manager, cv, train_set, hyper_params) {
  new_instance(
    'CVOptimizationContext',
    'optimization_manager' = optimization_manager,
    'cv'                   = cv,
    'train_set'            = train_set,
    'hyper_params'         = hyper_params
  )
}


CVOptimizationManager <- function(
  eval_metric_fn, 
  hyper_params_build_fn,
  on_after_cv_fn,
  build_train_set_fn,
  nfold      = 10, 
  nthread    = 24,
  stratified = TRUE
) {
  new_instance(
    'CVOptimizationManager',
    'nfold'                 = nfold,
    'nthread'               = nthread,
    'stratified'            = stratified,
    'eval_metric_fn'        = eval_metric_fn,
    'hyper_params_build_fn' = hyper_params_build_fn,
    'on_after_cv_fn'        = on_after_cv_fn,
    'build_train_set_fn'    = build_train_set_fn
  )
}

perform <- function(self, hiper_params) {
  print('Light GBM Cross Validation..')
  complete_hyper_params <- self$hyper_params_build_fn(hiper_params)
  
  print('Build train set..')
  train_set <- self$build_train_set_fn(complete_hyper_params)

  print('Start Cross Validation...')
  cv <- lgb.cv(
    eval            = self$eval_metric_fn,
    params          = complete_hyper_params,
    nthread         = self$nthread,
    folds           = self$folds,
    stratified      = self$stratified,
    data            = train_set
  )
  
  ctx <- CVOptimizationContext(
    self, 
    cv,
    train_set,
    complete_hyper_params
  )
  
  self$on_after_cv_fn(ctx)
}





