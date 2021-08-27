# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(dplyr, BiocManager, mdqc, caret, FactoMineR, factoextra, isotree)
# ------------------------------------------------------------------------------
#
#
#
#
pca <- function(df, scale = TRUE, robust="MVE", nsamp=10) {
  prcomp.robust(df, scale = scale, robust=robust, nsamp=nsamp)
}

plot_pca_original_variables <- function(df) PCA(df, graph = T)

plot_pca <- function(
  pca_result, 
  groups=NULL, 
  alpha=0.08, 
  title='', 
  ellipse = TRUE, 
  colours=c("green", "red"),
  labels=c("No", "Yes"),
  obs.scale = 1,
  var.scale = 1,
  varname.adjust = 1.5,
  varname.size = 3
) {
  ggbiplot(
    obs.scale=obs.scale,
    var.scale=var.scale,
    varname.adjust=varname.adjust,
    varname.size=varname.size,
    pca_result,
    alpha=alpha,
    groups=groups,
    ellipse=ellipse
  ) +
    scale_color_manual(
      name=title, 
      values=colours,
      labels=labels
    ) +
    theme(legend.direction ="horizontal", legend.position = "top")
}

isolation_forest_scores <- function(df, ntrees = 3, plot=TRUE) {
  scores <- isolation.forest(df, ntrees = 3, output_score=TRUE)$score
  if(plot) {
    boxplot(scores)
  }
  scores
}

filter_by_score <- function(df, max_score) {
  df %>% 
    dplyr::filter(score <= max_score) %>% 
    dplyr::select(-score)
}



