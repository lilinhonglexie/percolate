
#' Board Generation
#'
#' Randomly generate a legal, dry board per inputted instructions.
#'
#' @param n A positive integer denoting the dimension of the square board.
#' @param p A float between 0 to 1 that denotes the fraction
#' of the n^2 squares are blocked
#'
#' @return The n-by-n board matrix with floor(p*n^2) blocked squares
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

#' Board Validity Check
#'
#' Checks if a board matrix is valid. By definition, a valid board matrix is
#' a square matrix containing only 0, 1 and 2s. If the input is invalid, this
#' function throws an error, otherwise it returns TRUE.
#'
#' @param board_mat Board matrix to be checked.
#'
#' @return Either throws an error or returns TRUE
#' @export
#'
#' @examples is_valid(generate_board_mat())
is_valid <- function(board_mat){
  assertthat::assert_that(is.matrix(board_mat), msg="Invalid board.")
  assertthat::assert_that(length(dim(board_mat)) == 2, msg="Invalid board.")
  assertthat::assert_that(dim(board_mat)[1] == dim(board_mat)[2], msg="Invalid board.")
  assertthat::assert_that(sum(board_mat != 0 & board_mat != 1 & board_mat != 2) == 0,
                          msg="Invalid board.")
  TRUE
}

#' Dry Board Validity Check
#'
#' Checks if a valid board is dry -- contains only 0 and 1's
#'
#' @param board_mat A board that already passes the is_valid checks
#'
#' @return TRUE if the board is valid and dry
#' @export
#'
#' @examples is_valid_dry(board())
is_valid_dry <- function(board_mat){
  assertthat::assert_that(sum(board_mat != 0 & board_mat != 1) == 0)
  TRUE
}
