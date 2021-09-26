# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(caret)
# ------------------------------------------------------------------------------
#
#
#
#
find_high_correlated_columns <- function(df, cutoff=0.8) {
  columns <- findCorrelation(cor(df), cutoff=cutoff, verbose = T)
  names(df)[columns]
}
