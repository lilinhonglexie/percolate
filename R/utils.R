
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
  assertthat::assert_that(assertthat::is.number(n) && n > 0 && round(n) == n,
                          msg = "Invalid input n!")
  assertthat::assert_that(assertthat::is.number(p) && 0 <= p && p <= 1,
                          msg = "Invalid input p!")
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
#' @examples
#' is_valid(generate_board_mat()) # should return TRUE
#' is_valid(generate_board_mat(n=1)) # should return TRUE
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

#' Translate Text File to Board Objects
#'
#' @param file Path to text files that will be read in using readLines(). The vector
#' of character strings will contain correctly encoded boards, with a separator
#' "----" placed before and after each board.
#'
#' If the file's overal format does not meet the expectation, an error will be thrown.
#' If the file is formatted correctly overall, but a board has invalid encoding,
#' no error will occur but the board will be returned as NA rather than a board object.
#'
#' @return A list of board objects (or NA for invalid boards).
#' @export
#'
#' @examples
#' read_boards("https://raw.githubusercontent.com/benjaminleroy/36-350-summer-data/master/Week5/percolation_write_example.txt")
read_boards <- function(file){
  # remove empty lines
  file_content <- trimws(readLines(file))
  file_content <- file_content[file_content != ""]
  # check formatting
  board_sep <- "----"
  board_indices <- which(file_content == board_sep)
  assertthat::assert_that(board_indices[1] == 1 &&
                          board_indices[length(board_indices)] == length(file_content),
                          msg="Incorrect formatting.")
  n_board <- length(board_indices) - 1
  #board_dims <- file_content[board_indices[1:n_board] + 1]
  #actual_dims <- board_indices[-1] - board_indices[-(length(board_indices))] - 2
  #assertthat::assert_that(sum(board_dims != actual_dims) == 0,
                          #msg="Incorrect formatting.")
  # read boards
  n_board_seq <- 1:n_board
  boards <- lapply(n_board_seq, function(board_i){
    char_to_board(file_content[board_indices[board_i]:board_indices[board_i+1]])
    })
  boards
}

#' Translate a snippet of character strings to a board object
#'
#' @param boards_as_chars
#' Expected input format:
#'     ----
#'     4
#'     * . . *
#'     . . * *
#'     . * . .
#'     . . . .
#'     ----
#'
#' @return A board object as encoded by the inputted snippet.
#' @export
#'
#' @examples
char_to_board <- function(board_as_char){
  n <- board_as_char[2]
  if (grepl("\\D", n) || n <= 0) {
    return(NA)   # contains non-digit
  }
  n <- as.integer(n)
  board_rows_ls <- strsplit(board_as_char[3:(length(board_as_char)-1)], split=" ")

  if (length(board_rows_ls) != n || sum(sapply(board_rows_ls, length) != n) > 0){
    # boards with wrong formatting
    return(NA)
  } else {
    board_rows_ls <- lapply(board_rows_ls, char_row_to_board_row)
    if (sum(sapply(board_rows_ls, is.na)) > 0){
      # board rows with wrong formatting or unrecognized characters
      return(NA)
    } else {
      # make board objects
      board_mat <- do.call(rbind, board_rows_ls)   # list of vectors to matrix
      dimnames(board_mat) <- NULL
      return(board(board_mat))
    }
  }
}

#' Translate A Character Vector to A Board Row
#'
#' @param board_row_as_char A string vector of length n,
#' which is the dimension of the board. Only strings that encode recognized board
#' values are translated into numerics, otherwise NA is returned to denote that
#' the string does not correspond to any meaningful board value.
#'
#' @return A vector of 0, 1, or NA's of the same length as the input vector.
#' @export
#'
#' @examples
char_row_to_board_row <- function(board_row_as_char){
  sapply(board_row_as_char,
         function(board_square_as_char){
           if (board_square_as_char == "*"){
             return(0)
           } else if (board_square_as_char == "."){
             return(1)
           } else {
             return(NA)
           }
         })
}






