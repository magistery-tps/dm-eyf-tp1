# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(stringi, tidyverse, plotly)
# ------------------------------------------------------------------------------
#
#
#
#
options(warn=-1)

pie_plot <- function(data, title) {
  values = table(data)
  labels = paste(names(values), " (", values, ")", sep="")

  pie(values, labels = labels, main=title)  
}

ggpie_plot <- function(df, seg_label="", sum_label="") {
  p <- ggplot(df, aes(x="", y=Frequency, fill=Value)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    geom_text(
      aes(label = Frequency), 
      position = position_stack(vjust = 0.5), 
      color = "white"
    )+
    labs(
      x = NULL, 
      y = NULL, 
      fill = seg_label, 
      title = paste(sum_label, "por", seg_label, sep=" ")
    )  +
    theme_void()
  p
}
