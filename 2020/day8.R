library(stringr)
library(readr)

test_input <- c("nop +0",
"acc +1",
"jmp +4",
"acc +3",
"jmp -3",
"acc -99",
"acc +1",
"jmp -4",
"acc +6")


day8_1 <- function(input){
  instr <- str_split(input, " ")
  
  stack <- c(rep(TRUE, length(input)))
  trigger <-  TRUE
  
  position <- 1
  acc = 0

  while(trigger){
  
    stack[position] <- FALSE
    current <- instr[[position]]
    
    if(current[[1]] == "acc"){
      acc <- acc+parse_number(current[[2]])
      position <- position+1
    } else {
    
    position <- switch(current[[1]],
           "nop" = position +1,
           "jmp" = position + parse_number(current[[2]]))
    }
  
  if(!stack[position]){
    trigger <- FALSE
  }
    
  
  }
  acc

}

day8_1(test_input)

input <- read_lines("~/advent-of-code/2020/inputs/day8.txt")


day8_1(input)


#> For this section, I want to get a list of the instructions, in order
#> Then starting at the last one, flip the value of that and run the process again
#> Then repeat until completion
#> 

day8_1point5 <- function(input){
  instr <- input
  
  substack <- c(rep(TRUE, length(instr)))
  trigger <-  TRUE
  
  position <- 1
  acc = 0
  
  while(trigger){
    
    substack[position] <- FALSE
    current <- instr[[position]]
    
    if(current[[1]] == "acc"){
      acc <- acc+parse_number(current[[2]])
      position <- position+1
    } else {
      
      position <- switch(current[[1]],
                         "nop" = position +1,
                         "jmp" = position + parse_number(current[[2]]))
    }
    
    if(position > length(instr)){
      trigger <- FALSE
    } else if(!substack[position]){
      trigger <- FALSE
      acc <-  FALSE
    }
    
    
  }
  acc
  
}


day8_2 <- function(input){
  
  instr <- str_split(input, " ")
  
  stack <- c(rep(TRUE, length(input)))
  trigger <-  TRUE
  
  position <- 1
  
  output <- list()
  
  while(trigger){
    
    stack[position] <- FALSE
    current <- instr[[position]]
    
    if(current[[1]] == "acc"){
      
      position <- position+1
    } else {
      
      output[[length(output)+1]] <- list(current[[1]],position)
      
      position <- switch(current[[1]],
                         "nop" = position +1,
                         "jmp" = position + parse_number(current[[2]]))
    }
    
    if(!stack[position]){
      trigger <- FALSE
    }
    
    
  }
  
  
  for(case in rev(output)){
    instance <- instr
    
    new_case <- switch(case[[1]], 
                       "nop" = "jmp",
                       "jmp" = "nop")
    
    instance[[case[[2]]]][[1]] <- new_case
    
    answer <- day8_1point5(instance)
    
    
    if(answer){
      break
    }
    
  }
  answer
  
}

day8_2(test_input)
#8

day8_2(input)
