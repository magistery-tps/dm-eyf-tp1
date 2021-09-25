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
CVOptimizationManager <- function(
  train_set, 
  eval_metric_fn, 
  hyper_params_builder,
  on_after_cv_fn,
  nfold      = 10, 
  nthread    = 24,
  stratified = TRUE
) {
  new_instance(
    'LightGBMCorssValidationOptimizeManager',
    'nfold'                = nfold,
    'train_set'            = train_set,
    'eval_metric_fn'       = eval_metric_fn,
    'hyper_params_builder' = hyper_params_builder,
    'nthread'              = nthread,
    'stratified'           = stratified,
    'on_after_cv_fn'       = on_after_cv_fn
  )
}

perform <- function(self, hiper_params) {
  complete_hyper_params <- self$hyper_params_builder(hiper_params)

  print('Start Light GBM Cross Validation..')
  cv <- lgb.cv(
    eval            = self$eval_metric_fn,
    params          = complete_hyper_params,
    nthread         = self$nthread,
    nfold           = self$nfold,
    stratified      = self$stratified,
    data            = self$train_set
  )
  
  self$on_after_cv_fn(
    self, 
    cv,
    complete_hyper_params,
    hiper_params
  )
}





