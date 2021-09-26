print('Init data module...')
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('../import.R')
#
import('./data/correlation.R')
import('./data/csv.R')
import('./data/data-access.R')
import('./data/data-frame.R')
import('./data/importance.R')
import('./data/pca.R')
import('./data/scale.R')
# ------------------------------------------------------------------------------