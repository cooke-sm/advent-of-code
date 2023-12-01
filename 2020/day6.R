library(tidyverse)
test <- lapply(read_file("~/advent-of-code/2020/inputs/day6_test.txt"), stringr::str_split, "\r\n\r")[[1]][[1]]
input <- lapply(read_file("~/advent-of-code/2020/inputs/day6.txt"), stringr::str_split, "\n{2}")[[1]][[1]]



day6_1 <- function(input){
  split <- str_split(input, "\n|\r")
  
  groups <- lapply(split, \(x) str_extract(x, "[a-z]+") |> str_split("",simplify = TRUE))
  
  groups2 <- lapply(groups, \(x) x[!is.na(x) & (x != "")])
  
  sum(sapply(groups2, \(x) length(unique(x))))
  
  
}

day6_1(test)
#11

day6_1(input)
#6273


day6_2 <- function(input){
  split <- str_split(input, "\n|\r")
  groups <- lapply(split, \(x) str_extract(x, "[a-z]+") |> str_split(""))
  to_reduce <- lapply(groups, \(x) x[!is.na(x) & (x != "")])
  sum(unlist(lapply(to_reduce, \(x) Reduce(intersect, x) |> length())))
}

day6_2(test)
#6

day6_2(input)
#3254
