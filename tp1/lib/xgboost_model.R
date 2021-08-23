# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(
  this.path,
  ggplot,
  xgboost, 
  DiagrammeR, 
  stringr
)
setwd(this.path::this.dir())
# ------------------------------------------------------------------------------
#
#
#
#
feat        <- function(data) data %>% dplyr::select(-target)
target      <- function(data) data %>% dplyr::select(target) %>% pull()

xgboost_train <- function(
  data, 
  nthread     = 24, 
  max_depth   = 5, 
  nround      = 70, 
  verbose     = 1,
  eta         = 0.3, 
  alpha       = 0,
  gamma       = 0,
  eval_metric = 'logloss',
  objective   = "binary:logistic"
) {
  xgboost(
    data        = as.matrix(feat(data)),
    label       = target(data),
    max_depth   = max_depth,
    nround      = nround,
    eta         = eta,
    alpha       = alpha,
    gamma       = gamma,
    verbose     = verbose,
    nthread     = nthread,
    eval_metric = eval_metric,
    objective   = objective
  )
}


xgboost_cv <- function(
  data, 
  nthread     = 24, 
  max_depth   = 5, 
  nround      = 20, 
  verbose     = 1,
  eta         = 0.3, 
  alpha       = 0,
  gamma       = 0,
  nfold       = 10,
  eval_metric = 'logloss',
  objective   = "binary:logistic"
) {
  xgb.cv(
    data        = as.matrix(feat(data)),
    label       = target(data),
    max_depth   = max_depth,
    nround      = nround,
    eta         = eta,
    alpha       = alpha,
    gamma.      = gamma,
    verbose     = verbose,
    eval_metric = eval_metric,
    nfold       = nfold,
    objective   = objective
  )
}

plot_xgboost_cv_train_vs_val <- function(
  cv_result,
  sub_metric = 'mean',
  colors     = c('blue','darkgreen')
) {
  metric <- cv_result$params$eval_metric
  logs   <- cv_result$evaluation_log
  
  metric_label <- paste(str_to_title(metric), str_to_title(sub_metric))
  train_y_mean <- paste('train_', metric , '_', sub_metric, sep='')
  test_y_mean  <- paste('test_', metric , '_', sub_metric, sep='')
  title        <- paste(metric_label, ': Train vs. Validation', .sep='')
  
  train_y_mean_sym <- ensym(train_y_mean)
  test_y_mean_sym <- ensym(test_y_mean)
  
  ggplot(logs) + 
    geom_line(aes(x=iter, y={{train_y_mean_sym}}, colour='train'), show.legend=TRUE) +
    geom_line(aes(x=iter, y={{test_y_mean_sym}}, colour='test'), show.legend=TRUE) +
    xlab('Iteration') +
    ylab(metric_label) +
    theme(legend.position = "bottom") +
    scale_colour_manual(
      name="Sets",
      values=c('train'=colors[1], 'test'=colors[2]),
      labels=c('Train', 'Validation')
    ) +
    ggtitle(title)
}




map_it <- function(array, fn) {
  result <- c()
  for(e in array) {
    result <- c(result, fn(e))
  }
  result
}

f_beta_score_fn <- function(threshold = 0.025, beta = 2) {
  function(preds, dtrain) {
    y_true <- data.frame(val = getinfo(dtrain, "label")) %>% 
      dplyr::mutate(val = as.numeric(val)) %>%
      pull()
    
    y_pred <- data.frame(val = preds) %>% 
      dplyr::mutate(val = as.numeric(val > threshold)) %>%
      pull()
    
    score <- fbeta_score(y_pred, y_true, beta = beta)
    score <- if(is.na(score)) 0 else score
    
    list(metric = "f_beta_score", value = score)
  }
}
