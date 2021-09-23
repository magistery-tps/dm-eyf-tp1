dict_to_string <- function(dict, sep=', ') {
  result = ''
  for(name in names(params)) {
    entry <- paste(name, '=', params[name], sep='')
    result <- paste(result, entry, sep=sep)
  } 
  result
}