cat("\014")
rm(list=ls())
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, lightgbm, yaml, philentropy)
p_load_gh("siyuany/rpsi")
setwd(this.path::this.dir())
source('../../lib/import.R')
#
import('../src/common.R')
# ------------------------------------------------------------------------------
#
#
#
# -----------------------------------------------------------------------------
# Load & preprocess dataset
# -----------------------------------------------------------------------------
setwd(this.path::this.dir())
dev_set  <- preprocessing(load_train_set(), excludes = c('numero_de_cliente', 'foto_mes'))
test_set <- load_test_set()

train_set = lgb.Dataset(data  = data.matrix(feat(dev_set)), label = target(dev_set))

setwd(this.path::this.dir())
params <-yaml.load(read_yaml(paste('../../results/2021-09-26_00-04-57_bo-light-gbm_gain_9617916.66666667.yml')))


params$metric    <- 'auc'
params$verbosity <- 1
params$threshold <- NULL

model <- lightgbm(data = train_set, params = params)

tree <- lgb.importance(model)
lgb.plot.importance(tree)


features <- tree$Feature
important_features <- features[1:20]
str(dev_set %>% dplyr::select(important_features))

rm(train_set)
rm(model)
rm(tree)
cat("\014")

feature_values <-function(df, column, nsamples) {
  df %>%
    dplyr::select(column) %>%
    sample_n(nsamples) %>%
    replace(is.na(.), 0) %>%
    mutate_if(is.character, as.factor) %>%
    pull(column)
}

k_test <- function(
  columns,
  train,
  test,
  nsamples    = 100000,
  max_p_value = 0.05
) {
  result <- data.frame(
    feature            = character(), 
    k_test_p_value     = numeric(), 
    k_test_h0_rejected = logical()
  )
  for (column in columns) {
    train_feat <- feature_values(train, column, nsamples)
    test_feat  <- feature_values(test, column, nsamples)

    k_test_p_value <- ks.test(train_feat, test_feat)$p.value

    rm(train_feat)
    rm(test_feat)

    result <- result %>% add_row(
      feature            = column,
      k_test_p_value     = k_test_p_value,
      k_test_h0_rejected = k_test_p_value < max_p_value
    )
  }
  result %>% arrange(k_test_p_value)
}

# Usually we can believe the population stays the same as the past if psi is less than 0.1,
# https://rdrr.io/github/siyuany/rpsi/man/psi.html
PSI <- function(columns, train, test, nsamples=100000) {
  result <- data.frame(feature=character(), psi=numeric(), psi_change=logical())
  for (column in columns) {
    value <- tryCatch({
        train_feat <- feature_values(train, column, nsamples)
        test_feat  <- feature_values(test, column, nsamples)
        value <- rPSI::psi(train_feat, test_feat)
        rm(train_feat)
        rm(test_feat)
        value
      },
      error = function(e){
        print(paste('Error with "', column, '" column. ', e, sep=''))
        NA
      }
    ) 
    result <- result %>% add_row(
      feature    = column, 
      psi        = as.numeric(value), 
      psi_change = psi < 0.1
    )
  }
  result %>% 
    drop_na(psi)  %>% 
    arrange(desc(psi))
}


test_result <- k_test(important_features, dev_set, test_set)
psi_result  <- PSI(important_features, dev_set, test_set)
report      <- test_result %>% left_join(psi_result, by = 'feature')

fwrite(report, file=paste('../../reports/data_drift_report.csv', sep=''), sep=",")

quit( save="no")


