options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('./import.R')
#
import('./common/init.R')
import('./data/init.R')
import('./model/init.R')
import('./plot/init.R')
# ------------------------------------------------------------------------------