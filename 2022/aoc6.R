library(tidyverse)
library(zoo)

input <- readLines("~/advent-of-code/2022/inputs/day6.txt")

is_unique <- function(vect_input){
  return(length(unique(vect_input)) == length(vect_input))
}

non_repeat <- function(in_vect, n){
  q <- str_split(c(in_vect), "")[[1]]
  p <- tibble(q = q)
  p %>% 
    mutate(a = rollapplyr(q, width = n, is_unique, align = "right", fill = NA),
           row = row_number()) %>% 
    filter(a == TRUE) %>% 
    head(1)
}

#part1
non_repeat(input, 4)
#part2
non_repeat(input, 14)
