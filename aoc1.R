library(tidyverse)
input <- "~/advent-of-code/inputs/day1.txt"


#read the data in

data <- read_lines(file = input)

#set up a loop to sum the calories

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

#get the most calories
max(output)

# Get the top three
df <- tibble(output)

df %>% 
  arrange(desc(output)) %>% 
  head(3) %>% 
  summarise(sum(output))
  



