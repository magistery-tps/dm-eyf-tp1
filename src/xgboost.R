options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, yaml)
setwd(this.path::this.dir())
source('../lib/import.R')
import('./model/init.R')
# ------------------------------------------------------------------------------
#
#


train_model <- function(
  train_set,
  val_set,
  beta    = 2,
  params  = xgb_default_params(), 
  nrounds = 10,
  threshold = 0.025
) {
  # print(params)
  
  model <- xgboost_train(train_set, params, nrounds)
  
  # Evaluate metric on validation set...
  val_pred  <- xgboost_predict(model, feat(val_set), threshold = threshold)
  val_real  <- target(val_set)
  
  # Evaluate metric on training set...
  train_pred  <- xgboost_predict(model, feat(train_set), threshold = threshold)
  train_real  <- target(train_set)

  generate_metrics(train_pred, train_real, val_pred, val_real, beta)
}

generate_metrics <- function(train_pred, train_real, val_pred, val_real, beta) {
  val_score   <- fbeta_score(val_pred, val_real, beta=beta, show = F)
  train_score <- fbeta_score(train_pred, train_real, beta=beta, show = F)
  
  train_gain <- gain_score(train_pred, train_real)
  val_gain   <- gain_score(val_pred, val_real)
  
  train_auc <- auc(train_pred, train_real)
  val_auc   <- auc(val_pred, val_real)
  
  list(
    'train_f2_score' = train_score,
    'val_f2_score'   = val_score,
    'f2_score_diff'  = abs(train_score - val_score),
    'train_auc'      = train_auc, 
    'val_auc'        = val_auc, 
    'auc_diff'       = abs(train_auc - val_auc),
    'train_gain'     = currency(train_gain),
    'val_gain'       = currency(val_gain), 
    'gain_diff'      = currency(abs(train_gain - val_gain)),
    'val_tp'             = tp(val_pred, val_real),
    'val_fp'             = fp(val_pred, val_real),
    'val_tn'             = tn(val_pred, val_real),
    'val_fn'             = fn(val_pred, val_real)
  )
}

grid_search_fn <- function(
  beta,
  eta_values,
  max_depth_values, 
  nrounds_values,
  alpha_values,
  gamma_values
) {
  function(fold, train_set, val_set) {
    metrics <- NULL
    for(nrounds in nrounds_values) {
      for(max_depth in max_depth_values) {
        for(alpha in alpha_values) {
          for(gamma in gamma_values) {
            for(eta in eta_values) {
              params <- xgb_default_params()
              params$max_depth <- max_depth
              params$alpha     <- alpha       
              params$gamma     <- gamma
              params$eta       <- eta
              params$verbose   <- 0
              
              curr_metrics <- train_model(train_set, val_set, beta, params, nrounds)
  
              metrics_row <- c(
                list(
                  'fold'      = fold,
                  'max_depth' = max_depth, 
                  'nrounds'   = nrounds, 
                  'alpha'     = alpha, 
                  'gamma'     = gamma, 
                  'eta'       = eta
                ),
                curr_metrics
              )
              print(data.frame(metrics_row))
              
              metrics <- if(is.null(metrics)) data.frame(metrics_row) else rbind(metrics, metrics_row)
            }
          }
        }
      }
    }
    print(metrics)
    metrics
  }
}

mean_by_fold_and_params <- function(metrics) {
  metrics %>%
    replace(is.na(.), 0) %>%
    group_by(across(all_of(colnames(metrics)[2:6]))) %>% 
    summarise_at(vars(colnames(metrics)[7:15]), mean, na.rm = TRUE) %>%
    arrange(gain_diff)
}

gain <- function(true_positives, false_positives) {
  currency((48750 * true_positives) - (1250 * false_positives))
}

gain_score <- function(predictions, reality) {
  gain(tp(predictions, reality), fp(predictions, reality))
}
