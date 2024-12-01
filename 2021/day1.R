library(tidyverse)

test <- readLines("~/advent-of-code/2021/data/day1_test")
input <- readLines("~/advent-of-code/2021/data/day1")


day1_1 <- function(input){
  data <- tibble(x = input)
  
  data |> 
    mutate(y = case_when(lag(x) > x ~ 0,
                         lag(x) < x ~ 1,
                         TRUE ~ 1)) |> 
    summarise(sum(y))
  
}

day1_1(input)

day1_2 <- function(input){
  
  data <- tibble(x = as.numeric(input))
  
  data |> mutate(pre = lag(x),
                 post = lead(x)) |> 
    rowwise() |> 
    mutate(sum = sum(x,pre,post, na.rm = FALSE)) |> 
    ungroup() |> 
    mutate(ans = case_when(lag(sum) > sum ~ 0,
                           lag(sum) < sum ~ 1,
                           TRUE ~ 0)) |> 
    summarise(sum(ans))
  
}

day1_2(test)

day1_2(input)
