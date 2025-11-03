input <- readLines("~/advent-of-code/2022/inputs/day2.txt")

library(stringr)


result <- function(x,y){
  res <- (y-x) %% 3
  lookup <- c("0" = 3, "1" = 6, "2" = 0)

  lookup[paste(res)]+y

}

day2_1 <- function(input){

  dict <- c("A" = "1", "B" = "2", "C" = "3", "X" = "1", "Y" = "2", "Z" = "3")

  strat <- lapply(input, \(x) str_split(str_replace_all(x, dict), " "))

  Reduce( "+", lapply(strat, \(x) result(as.numeric(x[[1]][[1]]), as.numeric(x[[1]][[2]]))))
    
}

day2_1(input)
#14297

day2_2 <- function()


