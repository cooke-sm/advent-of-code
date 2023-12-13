library(tidyverse)
test <- readLines("~/advent-of-code/2023/data/day9_test.txt")
input <- readLines("~/advent-of-code/2023/data/day9_input.txt")

gaps <- function(input){
  #take the vector as input, find the gaps between each item
  
  output <- vector(mode = 'numeric')
  for(num in seq(1,length(input)-1)){
    output[length(output)+1] <- input[num+1]-input[num]
  }
  
  if(!any(as.logical(output))){
    
    out <- tail(input,1)
    
  } else {
    out <- tail(input, 1)+gaps(output) #recursion <3
  }
  
  out
}


day9_1 <- function(input){
  
  parsed <- lapply(input, \(x) as.numeric(str_split(x, " ")[[1]]))
  sum(sapply(parsed, gaps))
  
}

#answer
day9_1(input)


gaps2 <- function(input){
  #take the vector as input, find the gaps between each item
  
  output <- vector(mode = 'numeric')
  for(num in seq(1,length(input)-1)){
    output[length(output)+1] <- input[num+1]-input[num]
  }
  
  if(!any(as.logical(output))){
    
    out <- head(input,1)
    
  } else {
    
    out <- head(input, 1)-gaps2(output) #recursion <3

  }
  
  out
}

day9_2 <- function(input){
  
  parsed <- lapply(input, \(x) as.numeric(str_split(x, " ")[[1]]))
  sum(sapply(parsed, gaps2))
  
}

day9_2(input)

