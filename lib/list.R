dict_to_string <- function(dict, sep=', ') {
  result = ''
  for(name in names(dict)) {
    entry <- paste(name, '=', dict[name], sep='')
    result <- paste(result, entry, sep=sep)
  } 
  result
}