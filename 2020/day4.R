library(tidyverse)

test_input <- read_file("~/advent-of-code/2020/inputs/day4_test.txt")

checker <- function(list, fields){
  
  pass <-  TRUE
  
  for(field in fields){
    if(any(str_detect(list,field))){
      
    } else {
      pass = FALSE
      break
    }
  }
  pass
  
}

parse <- function(vector) {
  pairs <- str_split(vector, ":")
  key <- lapply(pairs, \(x) x[1])
  value <- lapply(pairs, \(x) x[2])
  names(value) <- key
  value
}


day4_1 <- function(input){
  
  fields <- c("byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:")
  
  split <- lapply(str_split(input, "\n{2}"), str_split, "(\\s|\\r)")[[1]]
  
  answer <- lapply(split, checker, fields)
  
  answer
  
}

day4_1(test_input)

input <- read_file("~/advent-of-code/2020/inputs/day4.txt")

day4_1(input)


day4_2 <- function(input){
  
  fields <- c("byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:")
  
  split <- lapply(str_split(input, "\n{2}"), str_split, "(\\s|\\r)")[[1]]
  
  valid <- lapply(split[day4_1(input)==TRUE], parse)
  bind_rows(lapply(valid, \(x) x[!is.na(x)])) |> 
    filter((byr <= 2002) &
             iyr
             (str_detect(hcl, "^#")) &
             )
}

test <- day4_2(input)

test
bind_rows(test[1:100])


