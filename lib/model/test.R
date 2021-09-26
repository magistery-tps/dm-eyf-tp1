# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(
  mvnormtest, 
  biotools, 
  car,
  DescTools
)
setwd(this.path::this.dir())
source('../import.R')
#
import('./data/data-frame.R')
# ------------------------------------------------------------------------------
#
#
#
#

#
# Test de Hoteling para comparación de medias de dos poblaciones normales
#
mult_hoteling_test <- function(df, target_col) {
    HotellingsT2Test(
      as.matrix(df %>% dplyr::select(-target_col)),
      data = df
    )
}

#
# Test de normalidad multi-variado
#
mult_shapiro_test <- function(features) {
  mshapiro.test(t(as.matrix(features)))
}

#
# Test de homocedasticidad multi-variado
#
multi_boxm_test <- function(features, target) boxM(features, target)

#
# Test de normalidad uni-variado. Calcula el test de normalidad para cada 
# columna del dataframe pasado como argumento.
#
uni_shapiro_test <- function(features) {
  r <- foreach_col(
    features, 
    function(features, col_index) {
      print(paste('=> Variable: ', colnames(features)[col_index], sep=''))
      print('')
      print(shapiro.test(features[, col_index]))
    }
  )
}