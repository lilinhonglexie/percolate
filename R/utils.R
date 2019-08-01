
library(assertthat)

#' Generate a legal board per inputted instructions.
#'
#' @param n A positive integer denoting the dimension of the square board.
#' @param p A float between 0 to 1 that denotes the fraction
#' of the n^2 squares are blocked
#'
#' @return The n-by-n board with floor(p*n^2) blocked squares
#'
#' @examples generate_board_mat(n = 8, p = 0.75)
#'
#' @export
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
