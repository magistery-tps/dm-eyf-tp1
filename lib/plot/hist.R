options(warn=-2)
# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, stringi, tidyverse, modeest, plotly)
setwd(this.path::this.dir())
source('../import.R')
#
import('./data/data-frame.R')
# ------------------------------------------------------------------------------
#
#
#
#
# -----------------------------------------------------------------------------
# Descripcion: Grafian con la libreria estandar un histograma desde un array
#              de valores.. Tambien Indica donde se encuentra: 
#              min, max, mean, median, media truncada y modas.
# -----------------------------------------------------------------------------
plot_hist <- function(
  values, 
  name, 
  xlab, 
  ylab = "Frecuencia",
  lwd = 3,
  legend_ = TRUE,
  missing_value = 0,
  truncated_mean_value=0.05
) {
  # Fill missing values...
  values[is.na(values)] <- missing_value
  
  # Plot histogram..
  hist(
    values, 
    col  = "deepskyblue", 
    main = sprintf("Distribución - %s", name), 
    xlab = name,
    ylab = ylab,
    freq = FALSE
  )
  
  # Plot measures of central tendency...
  lines(density(values),                         col = "chocolate3",      lwd = lwd)
  abline(v = mean(values),                       col = "black",           lwd = lwd)
  abline(v = mean(values, truncated_mean_value), col = "wheat3",          lwd = lwd)
  abline(v = median(values),                     col = "red",             lwd = lwd)
  abline(v = mfv(values),                        col = "blue",            lwd = lwd)
  abline(v = max(values),                        col = "darkolivegreen4", lwd = lwd)
  abline(v = min(values),                        col = "darkgoldenrod1",  lwd = lwd)
  
  # Plot legend...
  if (isTRUE(legend_)) {
    legend(
      x = "topright",
      c(
        "Densidad", "Media","Media Truncada", 
        "Mediana", "Moda", "Máximo", "Mínimo"
      ),
      col = c(
        "chocolate3", "black", "wheat3", "red", 
        "blue", "darkolivegreen4", "darkgoldenrod1"
      ),
      lwd = c(lwd, lwd, lwd, lwd, lwd, lwd, lwd),
      cex = 1
    )
  }
}



# -----------------------------------------------------------------------------
# Descripcion: Grafian con ggplot2 un histograma desde un array de valores.
#              Tambien Indica donde se encuentra: min, max, mean, median,
#              media truncada y modas.
# -----------------------------------------------------------------------------
gplot_hist <- function(
  values,
  ylab = "Frecuencia",
  name = "",
  line_size=1.05,
  truncated_mean_value=0.05,
  binwidth=1,
  linetype="solid"
) {
  df = as.data.frame(values)

  p <- ggplot(df, aes(x=values)) +
    geom_histogram(aes(y=..density..), color="darkblue", fill="lightblue", binwidth=binwidth) +
    geom_density(alpha=0.2, size=line_size) +

    # Plot measures of central tendency...
    geom_vline(aes(xintercept = mean(values), color='Media'), linetype = linetype, size=line_size) +
    geom_vline(aes(xintercept = mean(values, truncated_mean_value), color='Media Truncada'), linetype=linetype, size=line_size) +
    geom_vline(aes(xintercept = median(values), color='Mediana'), linetype=linetype, size=line_size) +
    geom_vline(aes(xintercept = max(values), color='Máximo'), linetype=linetype, size=line_size) +
    geom_vline(aes(xintercept = min(values), color='Mínimo'), linetype=linetype, size=line_size)

  for(var in mfv(df$values)) {
      p <- p + geom_vline(aes(xintercept = var, color='Moda'), linetype=linetype, size=line_size)
  }
  
  p <- p + scale_color_manual(
    name = "Medidas de tendencia central", 
    values = c(
      'Media'   = "black", 
      'Media Truncada' = 'wheat3',
      'Mediana' = 'red',
      'Máximo'  = 'darkolivegreen4',
      'Mínimo'  = 'darkgoldenrod1',
      'Moda' = 'blue'
    )
  )
  
  p <- p + labs(x=name, y = ylab, title = paste("Histograma", name, sep=" - "))

  p
}


# -----------------------------------------------------------------------------
# Descripcion: Grafian con ggplot2 un histograma desde una table de frecuencias
#              con las columnas: value y frequency. 
#              Tambien Indica donde se encuentra: min, max, mean y median.
# -----------------------------------------------------------------------------
gplot_hist_from_freq_table <- function(
  freq_table,
  ylab = "Frecuencia",
  name = "",
  line_size=1,
  binwidth=1,
  linetype="solid"
) {
  p <- ggplot(
      freq_table, 
      aes(x=value, y=frequency)
    ) +
    geom_histogram(
        aes(y=..density..), 
        color="darkblue",
        fill="lightblue",
        binwidth=binwidth
    ) +
    # Plot measures of central tendency...
    geom_vline(
      aes(xintercept = freq_table_mean(freq_table), color='Media'), 
      linetype = linetype, 
      size=line_size
    ) + 
    geom_vline(
        aes(xintercept = max(value), color='Máximo'), 
        linetype=linetype, 
        size=line_size
    ) +
    geom_vline(
        aes(xintercept = min(value), color='Mínimo'), 
        linetype=linetype, 
        size=line_size
    ) +
    geom_vline(
        aes(xintercept = median(value), color='Mediana'),
        linetype=linetype,
        size=line_size
    )

  p <- p + scale_color_manual(
    name = "Medidas de tendencia central", 
    values = c(
      'Media'   = "black",
      'Mediana' = 'blue',
      'Máximo'  = 'darkolivegreen4',
      'Mínimo'  = 'darkgoldenrod1'
    )
  )
  
  p + labs(x=name, y = ylab, title = paste("Histograma", name, sep=" - "))
}
