library(tidyverse)

test_input <- read_lines("~/advent-of-code/2020/inputs/day11_test")


matr <- matrix(unlist(str_split(test_input, "")), ncol = max(nchar(test_input)), nrow = length(test_input))



rules <- function(value){
  switch(value,
         "L" = 0,
         "#" = 1,)
}




