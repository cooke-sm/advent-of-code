library(tidyverse)
input <- readLines("~/advent-of-code/2023/data/day5_input.txt")
test <- readLines("~/advent-of-code/2023/data/day5_test.txt")

parser <- function(input) {
  ins_set <- input[[1]][input[[1]] != ""]
  from_to <- str_split(str_split(ins_set[1], " ")[[1]], "-to-")[[1]]

  charm <- do.call(rbind, lapply(ins_set[2:length(ins_set)], \(x) str_split(x, " ")[[1]]))
  matrix(as.numeric(as.character(charm)), nrow = nrow(charm))
}


checker <- function(seed, map) {
  not_in_map <- TRUE

  for (row in seq(1:nrow(map))) {
    
    source_range <- range(map[row, 2], map[row, 2] + (map[row, 3] - 1))
    dest_range <- range(map[row, 1], map[row, 1] + (map[row, 3] - 1))
    

    if (between(seed, source_range[1], source_range[2])) {
      new_seed <- (seed-source_range[1])+dest_range[1]
      not_in_map <- FALSE
    }
  }
  if (not_in_map) {
    # if it's not in the table, use the inital value
    new_seed <- seed
  }

  new_seed
}

day5_1 <- function(input) {
  data <- split(input, cumsum(input == ""))
  seeds <- as.numeric(str_extract_all(data[[1]], "\\d+")[[1]])
  instr <- data[2:length(data)]
  answer <- vector(mode = "numeric")

  for (seed in seeds) {
    output <- c(seed)

    for (i in seq(1, length(instr))) {
      output[length(output) + 1] <- checker(output[length(output)], parser(instr[i]))
    }
    answer[length(answer) + 1] <- output[length(output)]
  }
  min(answer)
}

#answer
day5_1(input)


day5_2 <- function(input) {
  data <- split(input, cumsum(input == ""))
  seeds <- str_extract_all(data[[1]], "\\d+ \\d+")[[1]]
  
  seed_pairs <- lapply(seeds, \(x) as.numeric(str_split(x, " ")[[1]]))
  instr <- data[2:length(data)]
  answer <- vector(mode = "numeric")
  
  for(seed in unlist(lapply(seed_pairs,\(x) seq(x[1], sum(x))))) {
    output <- c(seed)
    
    for (i in seq(1, length(instr))) {
      output[length(output) + 1] <- checker(output[length(output)], parser(instr[i]))
    }
    answer[length(answer) + 1] <- output[length(output)]
  }
  min(answer)
}

day5_2(input)

