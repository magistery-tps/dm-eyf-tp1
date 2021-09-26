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
