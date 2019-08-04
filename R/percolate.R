
percolate <- function(x, ...) UseMethod("percolate")

#' Board Percolation
#'
#' Percolate a dry board
#'
#' @param x A board object expected to pass both the board validity check and
#' dry board validity check.
#'
#' @return A list of percolation result.
#'
#' List element `result_board` is a board object (with the same blocking pattern
#' as the inputted board) that has been filled with water by percolation.
#'
#' List element `result` is a boolean suggesting whether water is able to percolates to
#' the bottom of the board
#'
#' @export
#'
#' @examples
#' mat_example_list <- list(matrix(c(1,1,1,1,0,
#'                                   0,0,0,1,0,
#'                                   1,1,1,1,0,
#'                                   0,1,0,0,0,
#'                                   0,1,1,1,1), 5, 5),
#'                          matrix(c(1,1,1,1,0,
#'                                   0,0,0,1,0,
#'                                   0,1,1,1,0,
#'                                   0,1,0,0,0,
#'                                   0,1,1,1,1), 5, 5),
#'                          matrix(c(1,1,1,1,0,
#'                                   0,0,0,1,0,
#'                                   0,1,1,0,0,
#'                                   0,1,0,0,0,
#'                                   0,1,1,1,1), 5, 5))
#' board_example_list <- lapply(mat_example_list, board)
#' percolate_result_list <- lapply(board_example_list, percolate)
#' # display boolean results
#' sapply(percolate_result_list, function(percolate_result){
#'   percolate_result$result
#'   })
#'
#' # plot 6 example boards in two rows
#' dry_board_vis_list <- lapply(board_example_list,
#'                              function(board){
#'                                plot(board, grid=TRUE)
#'                              })
#' percolate_result_vis_list <- lapply(percolate_result_list,
#'                                     function(percolate_result){
#'                                       plot(percolate_result$result_board, grid=TRUE)
#'                                     })
#' gridExtra::grid.arrange(grobs=c(dry_board_vis_list,percolate_result_vis_list),
#'                         ncol=3, nrow=2)
percolate.board <- function(x){
  is_valid(x)
  is_valid_dry(x)

  result_board <- x
  curr_result_board <- x
  curr_result_board[1,][curr_result_board[1,] == 1] <- 2 # fill the first row with water
  while (sum(curr_result_board != result_board) > 0){
    result_board <- curr_result_board
    curr_result_board <- percolate_fill(curr_result_board)
  }
  result <- sum(result_board[attr(result_board, "n"),] == 2) > 0
  return(list(result_board = result_board,
              result = result))
}

#' Board Percolation Single Iteration
#'
#' For each entry on the current board, percolate a single step (up, down, left,
#' right) if the entry contains water. This is a helper function of percolate.board
#'
#' @param curr_board A board with the latest board state.
#'
#' @return An updated board by performing a single percolation step on each entry
#' of the inputted board. If no more water could be filled into the board, the
#' returned board should be identical to the inputted board.
#'
#' @export
#'
#' @examples
#' curr_result_board <- board()
#' updated_result_board <- percolate_fill(curr_result_board)
#' if (sum(updated_result_board != curr_result_board) == 0){
#'    cat("Percolation completed!")
#' } else {
#'    cat("More percolation steps required to fill the board!")
#' }
#'
percolate_fill <- function(curr_board){
  n <- attr(curr_board, "n")
  for (i in 1:n){
    for (j in 1:n){
      if (curr_board[i,j] == 2){
        curr_board <- percolate_step(curr_board, i, j+1) # down
        curr_board <- percolate_step(curr_board, i-1, j) # left
        curr_board <- percolate_step(curr_board, i+1, j) # right
        curr_board <- percolate_step(curr_board, i, j-1) # up
      }
    }
  }
  curr_board
}

#' Board Percolation Single Entry
#'
#' Fill up the specified location with water, if possible (which means the entry is
#' not blocked). This is a helper function of percolate_fill
#'
#' @param board A board with the latest board state.
#' @param i Row index of the square to be percolated.
#' @param j Column index of the square to be percolated.
#'
#' @return A board with the updated board state. An identical board will be returned
#' if water could not be filled into the specified location on board.
#'
#' @export
#'
#' @examples percolate_step(board(), 1, 1)
percolate_step <- function(board, i, j){
  n <- attr(board, "n")
  if (1 <= i && i <= n &&
      1 <= j && j <= n &&
      board[i,j] == 1){
    board[i,j] <- 2
  }
  board
}
