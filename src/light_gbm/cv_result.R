CVResult <- function(ctx) {
  gain            <- unlist(ctx$cv$record_evals$valid$gain$eval)[ctx$cv$best_iter]
  normalized_gain <- gain * ctx$optimization_manager$nfold

  # Esta es la forma de devolver un parametro extra
  attr(normalized_gain ,"extras" )  <- list("num_iterations"= ctx$cv$best_iter)

  new_instance(
    'CVResult',
    'cv'              = ctx$cv,
    'gain'            = as.numeric(gain),
    'normalized_gain' = as.numeric(normalized_gain)
  )
}