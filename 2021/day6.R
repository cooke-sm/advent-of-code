test <- readLines("~/advent-of-code/2021/data/day6_test")
input <- readLines("~/advent-of-code/2021/data/day6")
library(stringr)


day6_1 <- function(input, days){
  squids <- as.numeric(strsplit(input, ",")[[1]])
  
  
  #day loop does two things
  #check to see if the day == the reproducing day for members of current
  #add in new squids
  for(day in seq(1,days)){
    
    babies <- length(squids[squids == 0])
    squids[squids == 0] <- 7
    
    if(babies >0){
      for(baby in seq(1,babies)){
        squids[length(squids)+1] <- 9
      }
    }
    
    squids <- squids - 1

  }
    
  
  #answer is the length of the 'current' list
  length(squids)
}

day6_1(input,80)
#365131

#should have seen this one coming

day6_2 <- function(input, days){
  squids <- table(factor((strsplit(input, ",")[[1]]), levels = 0:8))
  
  squids <- as.numeric(squids)
  
  #squids <- data.frame(stage = c(0:8))
  for(day in seq(1,days)){
    
    babies <- squids[1] #those who are at position 0
    squids[8] <- squids[8]+squids[1]
    
    squids <- c(squids[-1],0)
    squids[9] <- babies
    
  }
  sum(squids)
}

day6_2(test, 256)
#26984457539

format(day6_2(input, 256), scientific = FALSE)
#1650309278600



