# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
p_load(this.path, lightgbm)
setwd(this.path::this.dir())
source('../../import.R')
#
import('./common/object.R')
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
  build_hyper_params_fn,
  build_train_set_fn,
  on_before_cv_fn = NULL,
  on_after_cv_fn,
  nfold           = 10, 
  nthread         = 24,
  stratified      = TRUE
) {
  new_instance(
    'CVOptimizationManager',
    'nfold'                 = nfold,
    'nthread'               = nthread,
    'stratified'            = stratified,
    'eval_metric_fn'        = eval_metric_fn,
    'build_hyper_params_fn' = build_hyper_params_fn,
    'build_train_set_fn'    = build_train_set_fn,
    'on_before_cv_fn'       = on_before_cv_fn,
    'on_after_cv_fn'        = on_after_cv_fn
  )
}

perform <- function(self, hiper_params) {
  print('Call build train set event..')
  complete_hyper_params <- self$build_hyper_params_fn(hiper_params)
  
  print('Call build train set event..')
  train_set <- self$build_train_set_fn(complete_hyper_params)

  if(!is.null(self$on_before_cv_fn)) {
    print('Call on_before_cv_fn event...')
    ctx <- CVOptimizationContext(
      self, 
      NULL,
      train_set,
      complete_hyper_params
    )
    self$on_before_cv_fn(ctx)
  }

  print('Perform Light GBM Cross Validation...')
  cv <- lgb.cv(
    eval            = self$eval_metric_fn,
    params          = complete_hyper_params,
    nthread         = self$nthread,
    folds           = self$folds,
    stratified      = self$stratified,
    data            = train_set
  )
  
  print('Call on_after_cv_fn event...')
  ctx <- CVOptimizationContext(
    self, 
    cv,
    train_set,
    complete_hyper_params
  )
  
  self$on_after_cv_fn(ctx)
}