library("tidyverse")

seven_bit <- c(64,32,16,8,4,2,1)
three_bit <- seven_bit[5:7]

binaryfy <- function(input, one, zero){
  parse_number(str_replace(str_replace(input, one, "1"), zero, "0"))
}

test_input <- "FBFBBFFRLR"

day5_1 <- function(input){
  input <- str_split(input, "")[[1]]
  rows <- input[1:7]
  cols <- input[8:10]
  
  (sum(binaryfy(rows, "B", "F")*seven_bit)*8)+sum(binaryfy(cols, "R", "L")*three_bit)
  
}

day5_1(test_input)

input <- read_lines("~/advent-of-code/2020/inputs/day5.txt")

seats <- sapply(input, FUN = day5_1)

max(seats) #answer1 

sort(seats)

subtract <- seq(min(seats),max(seats))

setdiff(subtract, seats) #answer2







