# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(smotefamily)
# ------------------------------------------------------------------------------
#
#
#
#
smote_balance <- function(df, target, k=5) {
  result <- SMOTE(df, target, K=k)
  
  print(paste('Input:'))
  print(paste('- Positives:', length(result$orig_P$class)))
  print(paste('- Negatives:', length(result$orig_N$class)))
  
  positive_value <- result$orig_P$class[1]
  n_positives <- result$data %>% filter(class == positive_value) %>% tally() 
  n_negatives <- nrow(result$data) - n_positives  
  
  print(paste('Output:'))  
  print(paste('- Positives:', n_positives))
  print(paste('- Negatives:', n_negatives))
  
  result$data %>% dplyr::select(-class)
}