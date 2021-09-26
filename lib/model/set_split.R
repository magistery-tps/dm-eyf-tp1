# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(zeallot)
# ------------------------------------------------------------------------------
#
#
#
#
train_test_split <- function(df, train_size=.7, shuffle=TRUE) {
  if(shuffle) {
    train_ind <- sample(seq_len(nrow(df)), size = (nrow(df) * train_size))
    train_set <- df[train_ind, ]
    test_set  <- df[-train_ind, ]
  } else {
    train_set <- df[1:abs(nrow(df)*train_size), ]
    test_set  <- df[abs(nrow(df)*train_size):nrow(df), ]
  }

  print(paste('Train Set size:', nrow(train_set)))
  print(paste('Test set size:', nrow(test_set)))

  list(train_set, test_set)
}
