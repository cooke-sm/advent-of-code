library(tidyverse)
test <- readLines("~/advent-of-code/2023/data/day8_test.txt")
test2 <- readLines("~/advent-of-code/2023/data/day8_test2.txt")
input <- readLines("~/advent-of-code/2023/data/day8_input.txt")

list_wrap <- function(x,y){if(x%%y == 0){y} else {x%%y}}

day8_1 <- function(input){
  instructions <- as.numeric(str_split(str_replace_all(input[1], c("L" = "2", "R" = "3")), "")[[1]])
  
  nodes <- data.frame(nodes = str_replace_all(input[3:length(input)], "(\\(|\\))", "")) |> 
    separate(nodes, into = c("start","l","r"), sep = "( =|,)") |> 
    mutate(across(everything(), trimws))
  
  current <-  "AAA"
  steps <-  1
  
  while(current != "ZZZ"){
    
    direct <- instructions[list_wrap(steps,length(instructions))]
    current <- nodes[which(nodes$start == current),direct]
    steps = steps+1
    
  }
  steps-1
}

day8_1(input)

