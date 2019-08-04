
test_that("Board Percolation Basic Correctness Check", {
  # a board with all open sites
  my_board_1 <- board(matrix(1, 10, 10))
  my_board_1_result <- percolate(my_board_1)
  testthat::expect_equal(class(my_board_1_result), "list")
  testthat::expect_equal(length(my_board_1_result), 2)
  testthat::expect_equal(my_board_1_result$result, TRUE)   # percolate
  testthat::expect_equal(sum(my_board_1_result$result_board == 2), 100)  # all squares are wet

  # a board with all sites blocked
  my_board_2 <- board(matrix(0, 10, 10))
  my_board_2_result <- percolate(my_board_2)
  testthat::expect_equal(class(my_board_2_result), "list")
  testthat::expect_equal(length(my_board_2_result), 2)
  testthat::expect_equal(my_board_2_result$result, FALSE)   # does not percolate
  testthat::expect_equal(sum(my_board_2_result$result_board == 0), 100)  # all squares are still blocked

  # A board with all squares on the top row blocked
  my_board_3 <- board(n=10)
  my_board_3[1,] <- 0
  my_board_3 <- board(my_board_3)
  my_board_3_result <- percolate(my_board_3)
  testthat::expect_equal(class(my_board_3_result), "list")
  testthat::expect_equal(length(my_board_3_result), 2)
  testthat::expect_equal(my_board_3_result$result, FALSE)   # does not percolate
  testthat::expect_equal(sum(ifelse(my_board_3 == 0,
                               my_board_3 == my_board_3_result$result_board,
                               TRUE) != TRUE), 0)   # expect all blocking patterns to remain the same

  # A board with all squares on the bottom row blocked
  my_board_4 <- board(n=10)
  my_board_4[10,] <- 0
  my_board_4 <- board(my_board_4)
  my_board_4_result <- percolate(my_board_4)
  testthat::expect_equal(class(my_board_4_result), "list")
  testthat::expect_equal(length(my_board_4_result), 2)
  testthat::expect_equal(my_board_4_result$result, FALSE)   # does not percolate
  testthat::expect_equal(sum(ifelse(my_board_4 == 0,
                                    my_board_4 == my_board_4_result$result_board,
                                    TRUE) != TRUE), 0)   # expect all blocking patterns to remain the same
})

test_that("percolate.board() works with all the test cases",{
  load(url("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolate_test.Rdata"))

  your_result_list <- lapply(board_list, percolate)

  bool_vec <- sapply(1:length(result_list), function(x){
    your_board <- your_result_list[[x]]$result_board
    result_board <- result_list[[x]]$result_board

    identical(your_board, result_board) *
      (your_result_list[[x]]$result == result_list[[x]]$result)
  })

  expect_true(all(bool_vec))
})




