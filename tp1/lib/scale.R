max_min_scale_fn <- function(x) (x - min(x)) / (max(x) - min(x))

max_min_scale <- function(df) {
  r <- apply(df, 2, max_min_scale_fn)
  r
}


scale_columns <- function(df, columns, scaler_fn) {
  df %>% mutate_at(vars(columns), scaler_fn)
}
