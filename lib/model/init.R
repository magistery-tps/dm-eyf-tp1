print('Init model module...')
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('../import.R')

# Modeling tools
import('./model/save.R')
import('./model/set_split.R')
import('./model/models.R')
import('./model/test.R')
import('./model/metrics.R')
import('./model/balance.R')
import('./model/cross_validation.R')

# Optimization
import('./model/optimization/init.R')

# Models
import('./model/light_gbm_model.R')
import('./model/xgboost_model.R')
import('./model/kmeans_model.R')
import('./model/xda_model.R')
import('./model/hc_model.R')
# ------------------------------------------------------------------------------a