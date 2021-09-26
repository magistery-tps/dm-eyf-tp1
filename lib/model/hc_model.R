hc_predict <- function(model, ds, k) {
  hc_result <- data.frame(ds)
  hc_result$cluster <- cutree(model, k=k)
  hc_result
}