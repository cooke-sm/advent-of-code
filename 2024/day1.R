test <- readLines("~/advent-of-code/2024/data/day1_test", warn = FALSE)
input <- readLines("~/advent-of-code/2024/data/day1", warn = FALSE)
source("~/advent-of-code/utils/utils.R")


day1_1 <- function(input){
  
  a <- unlist(lapply(strsplit(input, "\\s+"), \(x) x[1]))
  b <- unlist(lapply(strsplit(input, "\\s+"), \(x) x[2]))
  
  sum(abs(sort(as.numeric(a))-sort(as.numeric(b))))
}

day1_1(test)
#11

day1_1(input)
#3569916

day1_2 <- function(input){
  
  a <- unlist(lapply(strsplit(input, "\\s+"), \(x) x[1]))
  b <- unlist(lapply(strsplit(input, "\\s+"), \(x) x[2]))
  
  tab <- table(b) #extract this so it doesn't compute it every time in lapply below
  
  sum(unlist(lapply(a, \(x) as.numeric(x)*tab[x])), na.rm = TRUE)
}

day1_2(test)
#31

day1_2(input)
#26407426