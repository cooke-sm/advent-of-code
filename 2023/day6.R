library(tidyverse)

input <- readLines("~/advent-of-code/2023/data/day6_input.txt")

z <- function(time,distance){
  hold_time = seq(0,time)
  options <- (time-hold_time)*hold_time
  
  sum(options > distance)
  
}

day6_1 <- function(input){
  data <- as.data.frame(do.call(cbind, lapply(str_split(input, "\\s+"), \(x) as.numeric(x[-1]))))
  
  
  prod(unlist(map2(data$V1, data$V2, z)))

}

day6_1(input)

day6_2 <- function(input){
  data <- as.numeric(lapply(str_extract_all(input, "\\d+"), str_c, collapse = ""))
  
  z(data[1], data[2])
}

day6_2(input)













