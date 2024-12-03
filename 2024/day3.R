test <- readLines("~/advent-of-code/2024/data/day3_test", warn = FALSE)
input <- readLines("~/advent-of-code/2024/data/day3", warn = FALSE)
library(stringr)


mul <- function(x,y){ 
  x*y
}

day3_1 <- function(input){
  
  mulled <- unlist(str_extract_all(input, "mul\\(\\d+,\\d+\\)"))
  sum(unlist(lapply(mulled, \(x) eval(parse(text = x)))))

}

day3_1(input)

day3_2 <- function(input){
  
  input <- stringr::str_c(input,collapse = "")
  split <- str_split(input, "don't\\(\\)")[[1]]
  start <- split[1]
  dos <- str_extract(split, "do\\(\\).+")
  day3_1(dos[!is.na(dos)])+day3_1(start)

}

day3_2(input)
#108830766









