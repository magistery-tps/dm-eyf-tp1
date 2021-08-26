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
#
import('./metrics.R')
# ------------------------------------------------------------------------------
#
#
#
#
index_of <- function(array, values) {
  results <- c()
  for(index in 1:length(array)) {
    if(array[index] %in% values) {
      results <- c(results, index)
    }
  }
  results
}

percent <- function(value, total) round((value/total)*100, 2)

#
# https://stackoverflow.com/questions/10399792/stratified-10-fold-cross-validation
#
cv_callback <- function(dataset, callback_fn, target='target', k=10) {
  p_load(caret)
  folds <- createFolds(factor(dataset[[target]]), k = k, list = FALSE)
  
  results <- NULL
  for(index in 1:k) {
    print(paste('Fold iter:', index))

    # Fold identifiers
    val_fold_num    <- c(index)
    train_fold_nums <- setdiff(seq(1,k), c(index))
    print(paste('Val fold:', val_fold_num, '- Train Fold:', toString(train_fold_nums)))

    # Set element indexes
    val_fold_indexes   <- index_of(folds, val_fold_num)
    train_fold_indexes <- index_of(folds, train_fold_nums)
    # print(paste('Val Index len:', length(val_fold_indexes), '- Train Index len:', length(train_fold_indexes)))

    # Sets
    val_set   <- dataset[val_fold_indexes,]
    train_set <- dataset[train_fold_indexes,]
    
    stratify_metrics(val_set, train_set)

    # print(paste('Callback', class(callback_fn)))
    result <- callback_fn(index, train_set, val_set)

    results <- if(is.null(results)) result else union_all(results, result)
  }
  results
}

