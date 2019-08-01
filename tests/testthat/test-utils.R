test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("Board generation check", {
  default_board <- generate_board_mat()
  expect_equal(class(default_board), "matrix")
  expect_equal(dim(default_board), c(5,5))
  expect_equal(sum(default_board != 0 &
                     default_board != 1), 0)

  alt_board <- generate_board_mat(n=10)
  expect_equal(class(alt_board), "matrix")
  expect_equal(dim(alt_board), c(10,10))
  expect_equal(sum(alt_board != 0 &
                     alt_board != 1), 0)

  alt_board_1 <- generate_board_mat(p=0)
  expect_equal(sum(alt_board_1), length(alt_board_1))
  alt_board_2 <- generate_board_mat(p=1)
  expect_equal(sum(alt_board_2), 0)

  expect_error(generate_board_mat(n = c(1,2)), "Invalid input n!")
  expect_error(generate_board_mat(n="asdf"), "Invalid input n!")
  expect_error(generate_board_mat(n = 5.4), "Invalid input n!")
  expect_error(generate_board_mat(n = -5), "Invalid input n!")
  expect_error(generate_board_mat(p = 5.4), "Invalid input p!")
  expect_error(generate_board_mat(p = -5), "Invalid input p!")
  expect_error(generate_board_mat(p = "hello"), "Invalid input p!")
  expect_error(generate_board_mat(p = c(1,1.4)), "Invalid input p!")
})

test_that("Is_valid for board check", {
  expect_equal(is_valid(generate_board_mat()), TRUE)
  expect_equal(is_valid(generate_board_mat(n=1)), TRUE)

  expect_error(is_valid(1), "Invalid board.")
  expect_error(is_valid(matrix(0, nrow = 2, ncol = 3)), "Invalid board.")
  expect_error(is_valid(matrix(2, nrow = 3, ncol = 3)), "Invalid board.")
})
