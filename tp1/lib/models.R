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
  xgboost, 
  DiagrammeR
)
setwd(this.path::this.dir())
source('./import.R')
#
import('./metrics.R')
import('./xgboost_model.R')
# ------------------------------------------------------------------------------
#
#
#
#


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


xda_predict <- function(
  model, 
  features, 
  threshold=.5, 
  min_threshold=.5, 
  positive=1, 
  negative=0
) {
  result <- predict(model, features)

  data.frame(result$posterior) %>%
    mutate(class = case_when(
      X1 >= threshold ~ positive,
      X0 >= threshold ~ negative,
      TRUE ~ -1
    )) %>%
    mutate(
      class = ifelse(
        class == -1, 
        case_when(
          X1 >= min_threshold ~ positive,
          X0 >= min_threshold ~ negative
        ),
        class
      )
    ) %>%
    dplyr::select(class) %>%
    pull()
}


xgboost_predict <- function(model, features, threshold=.5, positive=1, negative=0) {
  model_predict(model, as.matrix(features), threshold, positive, negative)
}

model_predict <- function(model, features, threshold=.5, positive=1, negative=0) {
  prediction <- predict(model, features)
  ifelse(prediction >= threshold, positive, negative)
}

kmeans_variable_clusters <- function(features, n_clusters) {
  km_result <- kmeans(t(scale(features)), n_clusters)
  
  df_variables <- data.frame(t(features))
  df_variables$cluster <- km_result$cluster
  
  index_as_column(df_variables) %>% 
    dplyr::select(cluster, index) %>%
    dplyr::rename(variable = index) %>%
    arrange(cluster) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(variable = toString(unique(variable)))  
}

kmeans_predict <- function(ds, scale_fn, features_fn, k) {
  model <- kmeans(scale_fn(features_fn(ds)), k)

  km_result <- data.frame(ds)
  km_result$cluster <- model$cluster
  km_result
}

hc_predict <- function(model, ds, k) {
  hc_result <- data.frame(ds)
  hc_result$cluster <- cutree(model, k=k)
  hc_result
}




