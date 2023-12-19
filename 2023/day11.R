library(tidyverse)

test <- as.matrix(do.call(rbind, lapply(str_split(str_replace_all(readLines("~/advent-of-code/2023/data/day11_test.txt"), c("#" = "1", "\\." = 0)), ""), as.numeric)), ncol = 10)

#find the coordinates
#change the coordinates to be transformed by the number of preceding spaces
#manhattan distance them


day11_1 <- function(input){

  coords <- data.frame(which(input == 1, arr.ind = TRUE)) |> 
    rename(row = 1, col = 2)
  
  blank_rows <- apply(input, 1, sum)
  blank_cols <- apply(input, 2, sum)
  
 coords |> 
   rowwise() |> 
   mutate(row = row + length(which(row >= which(blank_rows == 0))),
          col = col + length(which(col >= which(blank_cols == 0))))
}

day11_1(test)

str(x)











