# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path)
setwd(this.path::this.dir())
source('./import.R')

# Basic tools
import('./reflection.R')
import('./data-frame.R')
import('./scale.R')

import('./csv.R')
import('./plot.R')

# Exploratory analisys
import('./importance.R')
import('./correlation.R')
import('./pca.R')

# Model tools
import('./set_split.R')
import('./models.R')
import('./test.R')
import('./metrics.R')
import('./balance.R')
# ------------------------------------------------------------------------------