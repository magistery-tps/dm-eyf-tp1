print('Init common module...')
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('../import.R')
#
import('./common/currency.R')
import('./common/list.R')
import('./common/object.R')
import('./common/reflection.R')
# ------------------------------------------------------------------------------