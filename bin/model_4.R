rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, DiceKriging, mlrMBO, scales)
setwd(this.path::this.dir())
source('../lib/import.R')
import('../src/common.R')
import('../src/light_gbm/load.R')
# ------------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Load & preprocess dataset
# -----------------------------------------------------------------------------
# Load dev y test
setwd(this.path::this.dir())
raw_dev_set <- loadcsv("../dataset/paquete_premium_202009.csv")
test_set    <- loadcsv("../dataset/paquete_premium_202011.csv")
dev_set     <- preprocessing(raw_dev_set, excludes = excluded_columns)




train_set <- lgb.Dataset(
  data   = data.matrix(feat(dev_set)), 
  label  = target(dev_set),
  weight = dev_set %>% 
    mutate(weight = ifelse(target ==1, 1.0000001, 1.0)) %>% 
    dplyr::select(weight) %>%
    pull()
)
test_features <- test_set %>% 
  dplyr::select(-c(excluded_columns, 'clase_ternaria'))






eval_metric_fn_builder <- function(min_threshold) {
  function(probs, data) {
    labels  <- data$getinfo('label')
    weights <- data$getinfo('weight')
    
    # Aqui esta el inmoral uso de los pesos para calcular la ganancia correcta
    gain  <- sum( 
      (probs > min_threshold) * ifelse(labels== 1 & weights > 1, 48750, -1250 )
    )
    
    list("name"= "gain", "value"= gain, "higher_better"= TRUE)
  }  
}



GLOBAL_max_gain <- 0

on_after_cv_fn <- function(optimization_manager, cv, hyper_params, bo_hyper_params) {
  cat("\014")

  result <- CVResult(cv, optimization_manager$nfold)

  current_gain <- as.numeric(result$gain)
  
  if(current_gain > GLOBAL_max_gain) {
    GLOBAL_max_gain <<- current_gain
    
    print(paste('MAX GAIN:', GLOBAL_max_gain))
    
    hyper_params$early_stopping_rounds <- NULL
    hyper_params$num_iterations        <- result$cv$best_iter # Asigno el mejor num_iterations

    model <- lightgbm(
      data    = optimization_manager$train_set,
      param   = hyper_params,
      verbose = -100
    )

    test_pred <- light_gbm_predict(
      model,
      features  = data.matrix(test_features), 
      threshold = hyper_params$threshold
    )
 
    # Save prediction...
    save_result(
      test_set, 
      test_pred, 
      model_name='bo-light-gbm', 
      params=c(list(gain=current_gain), bo_hyper_params)
    )
  } else {
    print(paste('Gain does not improve! Gain:', current_gain))
  }
  
  result$gain
}


build_hyper_params <- function(hyper_params) {
  c(
    hyper_params, 
    list(
      objective             = "binary",
      metric                = "custom",
      first_metric_only     = TRUE,
      boost_from_average    = TRUE,
      feature_pre_filter    = FALSE,
      verbosity             = -100,
      seed                  = 999983,
      max_depth             = -1,   # -1 significa no limitar,  por ahora lo dejo fijo
      min_gain_to_split     = 0.0,  # Por ahora, lo dejo fijo
      lambda_l1             = 0.0,  # Por ahora, lo dejo fijo
      lambda_l2             = 0.0,  # Por ahora, lo dejo fijo
      max_bin               = 31,   # Por ahora, lo dejo fijo
      num_iterations        = 9999, # Un numero muy grande, lo limita early_stopping_rounds
      force_row_wise        = TRUE,  # Para que los alumnos no se atemoricen con tantos warning
      early_stopping_rounds = as.integer(50 + 5 / hyper_params$learning_rate)
    )
  )
}

optimization_manager <- CVOptimizationManager(
  train_set,
  eval_metric_fn       = eval_metric_fn_builder(min_threshold = 0.031), 
  hyper_params_builder = build_hyper_params,
  on_after_cv_fn       = on_after_cv_fn,
  nfold                = 10, 
  nthread              = 24,
  stratified           = TRUE
)







hyper_params  <- makeParamSet( 
  makeNumericParam("learning_rate",    lower = 0.01,  upper = 0.1),
  makeNumericParam("feature_fraction", lower = 0.2,   upper = 1.0),
  makeIntegerParam("min_data_in_leaf", lower = 0,     upper = 8000),
  makeIntegerParam("num_leaves",       lower = 16L,   upper = 1024L),
  makeNumericParam("threshold",        lower = 0.020, upper = 0.055)
)


objetive_fn <- makeSingleObjectiveFunction(
  fn                   = function(x) { perform(optimization_manager, x) }, 
  minimize             = FALSE,
  noisy                = TRUE,
  par.set              = hyper_params,
  has.simple.signature = FALSE  # Paso los parametros en una lista
)

bo_path <- paste('../BO.RDATA', sep='')

# Se graba cada 600 segundos
ctrl <- makeMBOControl(
  save.on.disk.at = 0:2, 
  save.file.path  = bo_path
) 

# Cantidad de iteraciones
ctrl <- setMBOControlTermination(ctrl, iters = 150)

ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# Establezco la funcion que busca el maximo
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype      = "matern3_2",
  control      = list(trace= TRUE)
)

if(!file.exists(bo_path)) {
  run <- mbo(objetive_fn, learner = surr.km,  control = ctrl)
} else {
  run <- mboContinue(bo_path)   #retomo en caso que ya exista
}


