print('Init plot module...')
options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, devtools, stringi, dplyr, WVPlots, DT, plotly, GGally, Hmisc, ggrepel, nortest)
p_load_gh("vqv/ggbiplot")
setwd(this.path::this.dir())
source('../import.R')
#
import('./plot/hist.R')
import('./plot/pie.R')
# ------------------------------------------------------------------------------
#
#
#
#
plot_correlations <- function(data) {
  cor_matrix = cor(data) 
  cor_matrix[lower.tri(cor_matrix, diag = TRUE)] <- NA
  plot_heatmap(
    cor_matrix, 
    colors = c('blue', 'white', 'green')
  )
}

plot_heatmap <- function(data, colors = c('white', 'red')) {
  plot_ly(
    z = data, 
    y = colnames(data),
    x = rownames(data),
    colors = colorRamp(colors),
    type = "heatmap"
  )
}

show_table <- function(table, page_size = 6, filter = 'top') {
  datatable(
    table, 
    rownames = FALSE, 
    filter=filter, 
    options = list(page_size = page_size, scrollX=T)
  )
}


box_plot <- function(data, horizontal = TRUE, xlab="", ylab="") {
  boxplot(
    data,
    xlab=xlab, 
    ylab=ylab,
    horizontal = horizontal,
    las=1,
    cex.lab=0.8, 
    cex.axis=0.6,
    pars=list(boxlwd = 2, boxwex=.8),
    col=colors()
  )
}

data.frame.num.hist <- function(df) {
  hist.data.frame(df %>% select(where(is.numeric)))
}

coparative_boxplot <- function(df, from_col=1, to_col) {
  par(mfcol = c(from_col, to_col))
  for (k in from_col:to_col){
    boxplot(df[k], main = names(df[k]))
    grid()
  }
}

comparative_histplot <- function(df, from_col=1, to_col) {
  df = scale(df[, seq(from_col, to_col)])

  column_names <- colnames(df)
  
  par(mfcol = c(from_col, to_col))
  
  for (k in from_col:to_col) {
    hist(
      df[,k],
      proba = T,
      main = column_names[k],
      10
    )
    x0 <- seq(
      min(df[, k]), 
      max(df[, k]), 
      le = 50
    )
    lines(
      x0, 
      dnorm(x0, mean(df[,k]), sd(df[,k])), 
      col = "red", 
      lwd = 2
    )
    grid()
  }
}


comparative_qqplot <-function(df, from_col=1, to_col) {
  df = scale(df[, seq(from_col, to_col)])
  
  
  column_names <- colnames(df)
  

  pval = list()

  par(mfcol = c(from_col, to_col))

  for (k in from_col:to_col){
    qqnorm(df[,k], main = column_names[k])
    qqline(df[,k],col="red") 

    pval[k] = ad.test(df[,k])$p.value
    
    grid()
  }

  pval
}
