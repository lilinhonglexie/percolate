
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
  assert_that(is.number(n) && n > 0 && round(n) == n, msg = "Invalid input n!")
  assert_that(is.number(p) && 0 <= p && p <= 1, msg = "Invalid input p!")
  n_blocked <- floor(p*n^2)
  blocked_ids <- sample(seq(n^2), n_blocked, replace = FALSE)
  result_vec <- integer(n^2) + 1
  result_vec[blocked_ids] <- 0
  result_mat <- matrix(result_vec, nrow = n, ncol = n)
  return(result_mat)
}

#' Checks if a board matrix is valid. By definition, a valid board matrix is
#' a square matrix containing only 0 or 1. If the input is invalid, this function
#' throws an error, otherwise it returns TRUE.
#'
#' @param board_mat Board matrix to be checked.
#'
#' @return Either throws an error or returns TRUE
#' @export
#'
#' @examples is_valid(generate_board_mat())
is_valid <- function(board_mat){
  assert_that(is.matrix(board_mat), msg="Invalid board.")
  assert_that(length(dim(board_mat)) == 2, msg="Invalid board.")
  assert_that(dim(board_mat)[1] == dim(board_mat)[2], msg="Invalid board.")
  assert_that(sum(board_mat != 0 & board_mat != 1) == 0, msg="Invalid board.")
  TRUE
}
