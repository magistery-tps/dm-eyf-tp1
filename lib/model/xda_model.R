xda_predict <- function(
  model, 
  features, 
  threshold=.5, 
  min_threshold=.5, 
  positive=1, 
  negative=0
) {
  result <- predict(model, features)

  data.frame(result$posterior) %>%
    mutate(class = case_when(
      X1 >= threshold ~ positive,
      X0 >= threshold ~ negative,
      TRUE ~ -1
    )) %>%
    mutate(
      class = ifelse(
        class == -1, 
        case_when(
          X1 >= min_threshold ~ positive,
          X0 >= min_threshold ~ negative
        ),
        class
      )
    ) %>%
    dplyr::select(class) %>%
    pull()
}
