# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, lightgbm, dplyr)
setwd(this.path::this.dir())
source('../import.R')
#
import('./common/object.R')
# ------------------------------------------------------------------------------
#
#
#
#
light_gbm_predict <- function(model, features, threshold = 0.031) {
  predictions <- predict(model, features)
  data.frame(target = predictions) %>% 
    dplyr::mutate(target = as.numeric(target > threshold)) %>%
    pull()
}
