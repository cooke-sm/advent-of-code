library(rstack)
library(tidyverse)

file = "~/advent-of-code/2022/inputs/day5.txt"

input <- read_lines_raw(file = file)
instructions <- read_lines(file = file, skip = 10)
instructions <- str_extract_all(instructions, "[0-9]+")

stackcheck <- function(stack){
  check = TRUE
  while(check == TRUE){
    if(stack$peek() == charToRaw(" ")){stack$pop()}
    else {check = FALSE}
  }
}

one <- stack$new()
two <- stack$new()
three <- stack$new()
four <- stack$new()
five <- stack$new()
six <- stack$new()
seven <- stack$new()
eight <- stack$new()
nine <- stack$new()

stacks <- c(one,two,three,four,five,six,seven,eight,nine)

for(i in c(8:1)){
  print(i)
  x <- input[[i]]
  
  stacks[[1]]$push(x[2])
  stacks[[2]]$push(x[6])
  stacks[[3]]$push(x[10])
  stacks[[4]]$push(x[14])
  stacks[[5]]$push(x[18])
  stacks[[6]]$push(x[22])
  stacks[[7]]$push(x[26])
  stacks[[8]]$push(x[30])
  stacks[[9]]$push(x[34])
}

for(i in seq_along(stacks)){
  stackcheck(stacks[[i]])
}

instruction_interpreter <- function(vector_input, stacks){
  vector_input <- as.integer(vector_input)
  n_iter <- vector_input[1]
  pop_stack <- stacks[[vector_input[2]]]
  push_stack <- stacks[[vector_input[3]]]
  
  for(i in seq(1:n_iter)){
    push_stack$push(pop_stack$pop())
  }
    
}

instruction_interpreter2 <- function(vector_input, stacks){
  vector_input <- as.integer(vector_input)
  n_iter <- vector_input[1]
  pop_stack <- stacks[[vector_input[2]]]
  push_stack <- stacks[[vector_input[3]]]
  middle_stack <- stack$new()
  
  for(i in seq(1:n_iter)){
    middle_stack$push(pop_stack$pop())
  }
  n_iter2 <- middle_stack$size()
  
  for(i in seq(1:n_iter2)){
    push_stack$push(middle_stack$pop())
  }
}

for(i in seq_along(instructions)){
  instruction_interpreter2(instructions[[i]],stacks)
}

answer <- c()

for(i in seq_along(stacks)){
  answer <- append(answer,rawToChar(stacks[[i]]$peek()))
}

cat(answer)




