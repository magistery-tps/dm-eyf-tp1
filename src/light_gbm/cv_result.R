CVResult <- function(cv, nfold) {
  gain            <- unlist(cv$record_evals$valid$gain$eval)[cv$best_iter]
  normalized_gain <- gain * nfold
  
  # Esta es la forma de devolver un parametro extra
  attr(normalized_gain ,"extras" )  <- list("num_iterations"= cv$best_iter)

  new_instance(
    'CVResult',
    'cv'              = cv,
    'gain'            = gain,
    'normalized_gain' = normalized_gain
  )
}