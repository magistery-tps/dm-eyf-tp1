# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(
  ROCit, 
  cvms, 
  pROC,
  cutpointr,
  ggimage, 
  rsvg, 
  performance, 
  see, 
  qqplotr,
  MLmetrics
)
# ------------------------------------------------------------------------------
#
#
#
#

# 
# Devuelve un dataframe con features + true(target) + predicion 
#
features_pred_true_df <- function(data, pred_target, target_col) {
  true_target <- label(test_set, target_col)
  features <- features(test_set, target_col)
  features$pred <- pred_target
  features$true <- true_target
  features
}


#
# Devuelve el datafram input (data) con las siguientes columnas:
#  - Columnas de features
#  - Columna con el valor real del target(true)
#  - Columna con el valor predicho (pred)
#  - Una columna categorica que define que tipo de resutado es: TP.FP, FN o FP.
# De esta manera es posible analizar los features de cada grupo de 
# resultados FP, TP, etc...
#
observations_cm <- function(
  data, 
  pred_target, 
  target_col, 
  positive_class=1, 
  negative_class=0
) {
  features_pred_true_df(data, pred_target, target_col) %>%
    mutate(result_type = case_when(
      pred == positive_class & pred == true ~ 'TP',
      pred == positive_class & pred != true ~ 'FP',
      pred == negative_class & pred == true ~ 'TN',
      pred == negative_class & pred != true ~ 'FN'
    ))
}


#
# Métrica F Beta score
#
fbeta_score <- function(prediction, reality, model="", positive=1, beta=1, show=TRUE) {
  score <- FBeta_Score(
    y_true   = reality,
    y_pred   = prediction,
    positive = positive, 
    beta     = beta
  )

  if(show) {
    print(paste(model, ' F', beta, 'Score: ', score, sep=''))
  } else {
    score 
  }
}

#
# Calcula la curva ROC
#
roc <- function(predictions, reality) {
  rocit(as.numeric(predictions), as.numeric(reality))
}

#
# Calcula la curva ROC
#
aur <- function(predictions, reality) {
  r <- roc(predictions, reality)
  r$AUC
}

#
# Grafica de la curva ROC.
#
plot_roc <- function(predictions, reality) {
  plot(roc(predictions, reality))
}

#
# Devuelve el numero de falsos negativos
#
fn <- function(predictions, reality) table(predictions, reality)[1, 2]

#
# Devuelve el numero de falsos positivos
#
fp <- function(predictions, reality) table(predictions, reality)[2, 1]

#
# Grafica de la matriz de confusión.
#
plot_cm <- function(predictions, reality) {
  cm <- confusion_matrix(targets=reality, prediction=predictions)
  plot_confusion_matrix(cm)
}


#
# Imprime la matriz de confusión.
#
plot_text_cm <- function(predictions, reality) {
  table(predictions, reality, dnn = c("Reality", "Prediction"))
}

#
# Calcula el mejor punto de corte.
#
best_roc_threshold <- function(predictions, reality) {
  df <- data.frame(
    prediction= as.numeric(predictions),
    reality=as.numeric(reality)
  )
  cp <- cutpointr(
    df, 
    prediction, 
    reality,
    method = maximize_metric, 
    metric = sum_sens_spec
  )
  plot(cp)
  summary(cp)
}


clustering_max_sil_k <- function(df, kmax=10, f="kmeans") {
  clustering_metrics(df, kmax, f) %>% 
    dplyr::mutate(k = row_number()) %>%
    dplyr::arrange(desc(sil)) %>%
    dplyr::select(k) %>%
    dplyr::pull() %>%
    first()
}

clustering_metrics <- function(datA_esc, kmax=10, f="kmeans") {
  sil = array()
  sse = array()
  
  datA_dist <- dist(
    datA_esc, 
    method = "euclidean", 
    diag = FALSE, 
    upper = FALSE,
    p = 2
  )
  
  for(i in  2:kmax) {
    #centroide: tipico kmeans
    if (strcmp(f,"kmeans")==TRUE) {
      CL     = kmeans(datA_esc,centers=i,nstart=50,iter.max = kmax)
      sse[i] = CL$tot.withinss 
      CL_sil = silhouette(CL$cluster, datA_dist)
      sil[i] = summary(CL_sil)$avg.width
    }
    
    #medoide: ojo porque este metodo tarda muchisimo
    if (strcmp(f,"pam")==TRUE){ 
      CL     = pam(x=datA_esc, k=i, diss = F, metric = "euclidean")
      sse[i] = CL$objective[1] 
      sil[i] = CL$silinfo$avg.width
    }
  }
  return(data.frame(sse,sil))
}

plot_sil_sse <- function(metrics, kmax) {
  par(mfrow=c(2,1))
  plot(
    2:kmax, 
    metrics$sil[2:kmax],
    col=1,
    type="b", 
    pch = 19, 
    frame = FALSE, 
    xlab="Number of clusters K",
    ylab="sil"
  )
  plot(
    2:kmax,
    metrics$sse[2:kmax],
    type="b",
    pch = 19,
    frame = FALSE,
    xlab="Number of clusters K",
    ylab="sse"
  )
  par(mfrow=c(1,1))
  grid()
}


#
# Grafica las métricas sil y sse para cada valor de k jasta kmax.
#
clustering_metrics_plot <- function(data, kmax=10, f="kmeans") {
  metrics <- clustering_metrics(data, kmax, f)
  plot_sil_sse(metrics, kmax)
}

plot_dendrogram <- function(hc_result, k) {
  plot(hc_result)
  rect.hclust(hc_result, k=k, border="red")
}






  

