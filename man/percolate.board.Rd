% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percolate.R
\name{percolate.board}
\alias{percolate.board}
\title{Board Percolation}
\usage{
\method{percolate}{board}(x)
}
\arguments{
\item{x}{A board object expected to pass both the board validity check and
dry board validity check.}
}
\value{
A list of percolation result.

List element `result_board` is a board object (with the same blocking pattern
as the inputted board) that has been filled with water by percolation.

List element `result` is a boolean suggesting whether water is able to percolates to
the bottom of the board
}
\description{
Percolate a dry board
}
\examples{
mat_example_list <- list(matrix(c(1,1,1,1,0,
                                  0,0,0,1,0,
                                  1,1,1,1,0,
                                  0,1,0,0,0,
                                  0,1,1,1,1), 5, 5),
                         matrix(c(1,1,1,1,0,
                                  0,0,0,1,0,
                                  0,1,1,1,0,
                                  0,1,0,0,0,
                                  0,1,1,1,1), 5, 5),
                         matrix(c(1,1,1,1,0,
                                  0,0,0,1,0,
                                  0,1,1,0,0,
                                  0,1,0,0,0,
                                  0,1,1,1,1), 5, 5))
board_example_list <- lapply(mat_example_list, board)
percolate_result_list <- lapply(board_example_list, percolate)

# display boolean results (TRUE, TRUE, FALSE)
# The first two boards should percolate, whereas the last board should not.
sapply(percolate_result_list, function(percolate_result){
  percolate_result$result
  })

# plot 6 example boards in two rows
dry_board_vis_list <- lapply(board_example_list,
                             function(board){
                               plot(board, grid=TRUE)
                             })
percolate_result_vis_list <- lapply(percolate_result_list,
                                    function(percolate_result){
                                      plot(percolate_result$result_board, grid=TRUE)
                                    })
gridExtra::grid.arrange(grobs=c(dry_board_vis_list,percolate_result_vis_list),
                        ncol=3, nrow=2)
}
