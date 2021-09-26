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
# ------------------------------------------------------------------------------
#
#
#
#
xgboost_predict <- function(model, features, threshold=.5, positive=1, negative=0) {
  model_predict(model, as.matrix(features), threshold, positive, negative)
}

xgboost_predict <- function(model, features, threshold = 0.025) {
  predictions <- predict(model, as.matrix(features))
  data.frame(target = predictions) %>% 
    dplyr::mutate(target = as.numeric(target > threshold)) %>%
    pull()
}

xgb_default_params <- function() {
  list(
    nthread     = 24, 
    max_depth   = 5,
    verbose     = 0,
    eta         = 1, 
    alpha       = 0,
    gamma       = 0,
    eval_metric = 'logloss',
    objective   = "binary:logistic"# ,
    #gpu_id      = 0,
    #tree_method = 'gpu_hist'
  )
}

xgboost_train <- function(data, params=xgb_default_params(), nrounds=10) {
  verbose <- params$verbose
  params$verbose <- NULL

  xgboost(
    data    = as.matrix(feat(data)),
    label   = target(data),
    params  = params,
    nrounds = nrounds,
    verbose = verbose
  )
}

xgboost_cv <- function(
  data,
  params  = xgb_default_params(), 
  nfold   = 10, 
  nrounds = 10
) {
  xgb.cv(
    data    = as.matrix(feat(data)),
    label   = target(data),
    nfold   = nfold,
    nrounds = nrounds,
    params  = params
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
    
    score <- fbeta_score(y_pred, y_true, beta = beta, show = F)
    score <- if(is.na(score)) 0 else score
    
    list(metric = "f_beta_score", value = score)
  }
}

