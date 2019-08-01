
test_that("Board constructor check.", {
  some_board <- generate_board_mat(n=5, p=0.25)
  some_board_from_func <- board(mat=some_board)
  expect_equivalent(some_board, unclass(some_board_from_func))
  expect_equal(attr(some_board_from_func, "n"), 5)
  expect_equal(attr(some_board_from_func, "p"), floor(0.25*5^2)/5^2)

  error_board <- generate_board_mat()
  error_board[1,1] <- 2
  expect_error(is_valid(error_board), "Invalid board.")
  expect_error(board(mat=error_board), "Invalid board.")

  sample_board_from_np <- board(n=10, p=0.3)
  expect_equal(is_valid(sample_board_from_np), TRUE)
  expect_equal(attr(sample_board_from_np, "n"), 10)
  expect_equal(attr(sample_board_from_np, "p"), 0.3)
})
