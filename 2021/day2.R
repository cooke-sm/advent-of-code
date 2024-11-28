library(tidyverse)
test <- readLines("~/advent-of-code/2021/data/day2_test")
input <- readLines("~/advent-of-code/2021/data/day2")

day2_1 <- function(input){
  
  data <- as_tibble(do.call(rbind,args = str_split(input, " ")),.name_repair = "unique")
  
  data <- data |> 
    rename(ins = 1, n = 2) |> 
    mutate(n = as.integer(n)) |> 
    mutate(n = case_when(ins == "up" ~ n*-1,
                         TRUE ~ n),
           dir = case_when(ins == "forward" ~ "h",
                           TRUE ~ "v")) |> 
    group_by(dir) |> 
    summarise(ans =sum(n))
  
  prod(data$ans)
  
}

day2_1(test)
#150

day2_1(input)
#1635930 

day2_2 <- function(input){
  
  data <- as_tibble(do.call(rbind,args = str_split(input, " ")),.name_repair = "unique")
  
  data <- data |> 
    rename(ins = 1, n = 2) |> 
    mutate(n = as.integer(n)) |> 
    mutate(n = case_when(ins == "up" ~ n*-1,
                         TRUE ~ n),
           dir = case_when(ins == "forward" ~ "h",
                           TRUE ~ "v"),
           aim = cumsum(ifelse(dir == "v", n, 0))) |> 
    filter(dir == "h") |> 
    mutate(depth_inc = n*aim,
           depth = cumsum(depth_inc),
           h = cumsum(n))
  
  prod(data[nrow(data),] |> select(depth,h) |> unlist())
  
  
}


day2_2(input)
#1781819478







