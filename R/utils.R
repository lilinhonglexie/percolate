
library(assertthat)

generate_board_mat <- function(n=5, p=0.25){
  assert_that(n > 0)
  assert_that(0 <= p && p <= 1)
  n_blocked <- floor(p*n^2)
  blocked_ids <- sample(seq(n^2), n_blocked, replace = FALSE)
  result_vec <- integer(n^2)
  result_vec[blocked_ids] <- 1
  result_mat <- matrix(result_vec, nrow = n, ncol = n)
  return(result_mat)
}

generate_board_mat()
generate_board_mat(n = 8, p = 0.75)
