library(tidyverse)

test <- as.matrix(do.call(rbind, lapply(str_split(str_replace_all(readLines("~/advent-of-code/2023/data/day11_test.txt"), c("#" = "1", "\\." = 0)), ""), as.numeric)), ncol = 10)
input <- as.matrix(do.call(rbind, lapply(str_split(str_replace_all(readLines("~/advent-of-code/2023/data/day11_input.txt"), c("#" = "1", "\\." = 0)), ""), as.numeric)), ncol = 94)


manhattan <- function(i, j) {
  i <- unlist(i)
  j <- unlist(j)

  abs(i[1] - j[1]) + abs(i[2] - j[2])
}

day11_1 <- function(input) {
  coords <- data.frame(which(input == 1, arr.ind = TRUE)) |>
    rename(row = 1, col = 2)

  blank_rows <- apply(input, 1, sum)
  blank_cols <- apply(input, 2, sum)

  coords2 <- coords |>
    rowwise() |>
    mutate(
      row = row + length(which(row >= which(blank_rows == 0))),
      col = col + length(which(col >= which(blank_cols == 0)))
    ) |>
    mutate(coord = list(c(row, col))) |>
    select(coord)

  dist <- outer(coords2[[1]], coords2[[1]], Vectorize(manhattan))
  dist[upper.tri(dist) | dist == 0] <- NA

  sum(dist, na.rm = TRUE)
  
}

day11_1(input)

day11_2 <- function(input){
  
  coords <- data.frame(which(input == 1, arr.ind = TRUE)) |> 
    rename(row = 1, col = 2)
  
  blank_rows <- apply(input, 1, sum)
  blank_cols <- apply(input, 2, sum)
  
  coords2 <- coords |> 
    rowwise() |> 
    mutate(row = row + (length(which(row >= which(blank_rows == 0)))*(999999)),
           col = col + (length(which(col >= which(blank_cols == 0)))*(999999))) |> 
    mutate(coord = list(c(row,col))) |> 
    select(coord)
  
  dist <- outer(coords2[[1]],coords2[[1]], Vectorize(manhattan))
  dist[upper.tri(dist)|dist==0] <- NA
  
  sum(dist, na.rm = TRUE)
}

day11_2(input)


