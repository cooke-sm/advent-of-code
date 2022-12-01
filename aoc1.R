library(tidyverse)
input <- "~/advent-of-code/inputs/day1.txt"


data <- read_lines(file = input)


output <- vector(mode = "integer")
elf <- vector(mode = "integer" )

for(i in seq_along(data)){
  
  calories <- as.numeric(data[i])
  
  if(is.na(calories)) {
    elf <- sum(elf)
    output <- append(output, elf)
    elf <- vector(mode = "integer")
  } else {elf <- append(elf,calories)}
  
}

max(output)
  



