test <- readLines("~/advent-of-code/2024/data/day4_test")
input <- readLines("~/advent-of-code/2024/data/day4")


day4_1GPT <- function(input) {
  # Split the input into a grid
  grid <- unlist(strsplit(input, "\n"))
  
  
  # Convert the grid into a matrix for easier manipulation
  n <- length(grid)
  m <- nchar(grid[1])
  matrix_grid <- matrix(unlist(strsplit(grid, "")), nrow = n, byrow = TRUE)
  
  # Function to check if a word appears in a vector
  count_in_vector <- function(vec, word) {
    count <- 0
    word_length <- nchar(word)
    for (i in 1:(length(vec) - word_length + 1)) {
      if (paste0(vec[i:(i + word_length - 1)], collapse = "") == word) {
        count <- count + 1
      }
    }
    return(count)
  }
  
  # Function to find all occurrences of the word in all directions
  count_word <- function(grid, word) {
    total_count <- 0
    word_length <- nchar(word)
    rev_word <- paste0(rev(unlist(strsplit(word, ""))), collapse = "")
    
    # Horizontal and reversed horizontal
    for (i in 1:nrow(grid)) {
      total_count <- total_count + count_in_vector(grid[i, ], word)
      total_count <- total_count + count_in_vector(grid[i, ], rev_word)
    }
    
    # Vertical and reversed vertical
    for (j in 1:ncol(grid)) {
      total_count <- total_count + count_in_vector(grid[, j], word)
      total_count <- total_count + count_in_vector(grid[, j], rev_word)
    }
    
    # Diagonal directions
    diagonals <- list()
    
    # Collect diagonals from top-left to bottom-right and bottom-left to top-right
    for (k in 1:(nrow(grid) + ncol(grid) - 1)) {
      top_left_diag <- c()
      bottom_left_diag <- c()
      for (i in 1:nrow(grid)) {
        j1 <- k - i + 1
        j2 <- i - k + ncol(grid)
        if (j1 >= 1 && j1 <= ncol(grid)) {
          top_left_diag <- c(top_left_diag, grid[i, j1])
        }
        if (j2 >= 1 && j2 <= ncol(grid)) {
          bottom_left_diag <- c(bottom_left_diag, grid[i, j2])
        }
      }
      if (length(top_left_diag) >= word_length) {
        diagonals[[length(diagonals) + 1]] <- top_left_diag
      }
      if (length(bottom_left_diag) >= word_length) {
        diagonals[[length(diagonals) + 1]] <- bottom_left_diag
      }
    }
    
    # Count occurrences in diagonals
    for (diag in diagonals) {
      total_count <- total_count + count_in_vector(diag, word)
      total_count <- total_count + count_in_vector(diag, rev_word)
    }
    
    return(total_count)
  }
  
  # Count occurrences of "XMAS"
  return(count_word(matrix_grid, "XMAS"))
}


day4_1GPT(test)
#18

day4_1GPT(input)
#2599

day4_2GPT <- function(input) {
  # Parse the input grid
  grid <- unlist(strsplit(input, "\n"))
  n <- length(grid)
  m <- nchar(grid[1])
  matrix_grid <- matrix(unlist(strsplit(grid, "")), nrow = n, byrow = TRUE)
  
  # Function to check for a valid "MAS" or "SAM" pattern on a diagonal
  valid_mas <- function(chars) {
    word <- "MAS"
    rev_word <- paste0(rev(strsplit(word, "")[[1]]), collapse = "")
    return(paste0(chars, collapse = "") %in% c(word, rev_word))
  }
  
  # Count all valid X-MAS patterns
  total_count <- 0
  
  for (x in 2:(n - 1)) {         # Center must be at least 1 row/column from edges
    for (y in 2:(m - 1)) {
      # The center of the X-MAS must be 'A'
      if (matrix_grid[x, y] == "A") {
        # Extract characters for both diagonals
        diag1 <- c(matrix_grid[x - 1, y - 1], matrix_grid[x, y], matrix_grid[x + 1, y + 1])
        diag2 <- c(matrix_grid[x - 1, y + 1], matrix_grid[x, y], matrix_grid[x + 1, y - 1])
        
        # Check if both diagonals form valid "MAS" or "SAM" patterns
        if (valid_mas(diag1) && valid_mas(diag2)) {
          total_count <- total_count + 1
        }
      }
    }
  }
  
  return(total_count)
}

day4_2GPT(test)

day4_2GPT(input)
