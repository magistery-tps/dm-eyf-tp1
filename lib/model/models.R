# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(
  this.path, 
  MASS,
  stats,
  klaR,
  e1071,
  cluster,
  pracma,
  mvnormtest,
  DiagrammeR
)
setwd(this.path::this.dir())
source('../import.R')
#
import('./model/metrics.R')
# ------------------------------------------------------------------------------
#
#
#
#
feat        <- function(data) data %>% dplyr::select(-target)
target      <- function(data) data %>% dplyr::select(target) %>% pull()


#
# Selecciona el punto de curte que maximice el AUR del modelo
#
search_max_aur_threshold <- function(
  model, 
  features,
  target,
  perdict_callback,
  thresholds = seq(0.01, 0.99, 0.01)
) {
  best_threshold <- 0.5
  max_aur <- 0.5

  for(threshold in thresholds) {
    predictions <- perdict_callback(model, features, threshold)
    current_aur <- roc(predictions, target)$AUC[1]

    if (current_aur > max_aur) {
      best_threshold <- threshold
      max_aur <- current_aur
    }
  }

  best_threshold
}

search_min_fn_threshold <- function(
  model, 
  features,
  target,
  perdict_callback,
  thresholds = seq(0.01, 0.99, 0.01)
) {
  best_threshold <- 0.5
  min_fn <- 10000000000

  for(threshold in thresholds) {
    predictions <- perdict_callback(model, features, threshold)
    current_fn <- fn(predictions, target)

    if (current_fn < min_fn) {
      best_threshold <- threshold
      min_fn <- current_fn
    }
  }
  
  best_threshold
}


model_predict <- function(model, features, threshold=.5, positive=1, negative=0) {
  prediction <- predict(model, features)
  ifelse(prediction >= threshold, positive, negative)
}