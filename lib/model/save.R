# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, yaml, data.table)
# ------------------------------------------------------------------------------
#
#
#
#
default_filename_fn <- function(path, model_name, params) {
  paste(
    path, 
    '/', 
    strftime(Sys.time(), format="%Y-%m-%d_%H-%M-%S"),
    '_',
    model_name,
    '_params_',
    dict_to_string(params),
    sep=''
  )
}

save_model_result <- function(
  result,
  path         ='../../results',
  model_name   = 'model',
  hyper_params = list(),
  filename_fn  = default_filename_fn
) {
  filename <- filename_fn(path, model_name, hyper_params)
  fwrite(result, file=paste(filename, '.csv', sep=''), sep=",")
  write_yaml(as.yaml(hyper_params), file=paste(filename, '.yml', sep=''))
}