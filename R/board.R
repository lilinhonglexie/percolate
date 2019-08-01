
#' Class constructor function for the class "board".
#'
#' @param mat If not NULL, contains a board matrix.
#' @param n Dimension of the square board matrix. Used only when mat == NULL.
#' @param p A float between 0 to 1 that denotes the fraction
#' of the n^2 squares are blocked. Used only when mat == NULL.
#'
#' @return The generated board object per inputted instructions.
#' @export
#'
#' @examples board(mat=generate_board_mat())
board <- function(mat = NULL, n = 5, p = .25){
  if (is.null(mat)){
    mat <- generate_board_mat(n, p)
    assert_that(is_valid(mat))
  } else {
    assert_that(is_valid(mat))
    n <- dim(mat)[1]
    p <- (length(mat) - sum(mat))/length(mat)  # empirical percentage of 0's
  }
  attr(mat, "n") <- n
  attr(mat, "p") <- p
  class(mat) <- c("board", "matrix")
  mat
}
