rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, DiceKriging, mlrMBO, scales)
setwd(this.path::this.dir())
source('../lib/import.R')
import('./common.R')
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

bo_iterations <-  150
nfold         <- 10
hyper_params  <- makeParamSet( 
  makeNumericParam("learning_rate",    lower= 0.01 , upper= 0.1),
  makeNumericParam("feature_fraction", lower= 0.2  , upper= 1.0),
  makeIntegerParam("min_data_in_leaf", lower= 0    , upper= 8000),
  makeIntegerParam("num_leaves",       lower=16L   , upper= 1024L),
  makeNumericParam("threshold",        lower= 0.020, upper= 0.055)
)


build_metric_fn <- function(min_threshold = 0.025) {
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


optimization_fn <- function(x) {
  cv_result <- lgb.cv(
    #  device          = "gpu",
    #  gpu_platform_id = 0,
    #  gpu_device_id   = 0,
    #  gpu_use_dp      = TRUE,
    eval            = build_metric_fn(),
    params          = build_hyper_params(x),
    nthread         = 24,
    nfold           = nfold,
    stratified      = TRUE,
    data            = train_set
  )

  gain  <- unlist(cv_result$record_evals$valid$gain$eval)[ cv_result$best_iter ]
  
  normalized_gain <- gain * nfold

    # Esta es la forma de devolver un parametro extra
  attr(normalized_gain ,"extras" )  <- list("num_iterations"= cv_result$best_iter)

  gain
}



objetive_fun  <- makeSingleObjectiveFunction(
  fn       = optimization_fn, 
  minimize = FALSE,
  noisy    = TRUE,
  par.set  = hyper_params,
  has.simple.signature = FALSE  # Paso los parametros en una lista
)

ctrl  <- makeMBOControl(
  save.on.disk.at.time = 600, 
  save.file.path       = '../logs/log'  # Se graba cada 600 segundos
) 

ctrl  <- setMBOControlTermination(ctrl, iters = bo_iterations) # Cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# Establezco la funcion que busca el maximo
surr.km  <- makeLearner(
  "regr.km", 
  predict.type = "se", 
  covtype      = "matern3_2", 
  control      = list(trace= TRUE)
)

run  <- mbo(objetive_fun, learner = surr.km,  control = ctrl)

