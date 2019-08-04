
#' Plot a board based on the inputted board matrix.
#' Different values in the matrix represent squares of different colors.
#' 0: black; 1: white; 2: lightblue
#'
#' @param board: a board object. An error will occur if the inputted board is
#'               invalid.
#' @param grid: if TRUE, the squares are plotted with grey, dashed outlines
#'
#' @return The ggplot object visualizing the board.
#' @export
#'
#' @examples
#' board_example <- board(matrix(c(0,1,1,1,0,0,1,1,0,1,0,0,1,0,0,0,0,0,2,2,
#'                                 2,2,2,2,0), 5, 5))
#' board_example2 <- board(matrix(c(0,1,1,1,0,0,1,1,0,1,0,0,1,0,0,0,0,0,1,1,
#'                                  1,1,1,1,0), 5, 5))
#' board_example3 <- board(matrix(c(0,2,2,2,0,0,2,2,0,2,0,0,2,0,0,0,0,0,2,2,
#'                                  2,2,2,2,0), 5, 5))
#' board_example4 <- board(matrix(c(2, 0, 0, 2, 0, 0, 2, 2, 0, 0, 2, 2, 2, 0, 0,
#' 2, 2, 0, 0, 1,0, 2, 0, 0, 0, 0, 2, 0, 0, 0,0, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 2,
#' 0, 0, 0, 0, 1, 0, 0, 0, 2, 2, 2, 0, 1, 0, 0, 1, 1, 1, 0, 2, 2, 0, 1, 1, 0, 0,
#' 1, 0, 1, 0, 2, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0,
#' 0, 1, 0, 1, 0, 0), 10, 10))
#' plot.board(board_example4, grid=TRUE)
plot.board <- function(board, grid){
  is_valid(board)
  n <- attr(board, "n")
  board_outline <- ifelse(grid, "dashed", "blank")
  board_df <- reshape2::melt(board) %>%
    dplyr::mutate(x = Var2,
                  y = n - Var1 + 1,
                  state = as.factor(value))
  board_df %>%
    ggplot2::ggplot() +
    ggplot2::geom_tile(ggplot2::aes(x=x, y=y, fill=state),
                       linetype=board_outline, color="grey", size=0.5) +
    ggplot2::labs(title = sprintf("size: %s", n)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust=0.5)) +
    ggplot2::scale_fill_manual(
      values=c("0"="black", "1"="white", "2"="lightblue3"))
}

