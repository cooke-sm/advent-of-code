library(tidyverse)

test <- readLines("~/advent-of-code/2023/data/day7_test.txt")
input <- readLines("~/advent-of-code/2023/data/day7_input.txt")

hand_value <- function(hand){
  split <- str_split(hand, "")[[1]]
  hand_rank <- sum(13**table(split))
  hand_score <- lapply(split, match, c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A"))
  
  unlist(c(hand_rank, hand_score))
}

hand_value2 <- function(hand){
  split <- str_split(hand, "")[[1]]
  
  no_joke <- table(split[which(split!="J")]) 
  
  key <- if(sum(no_joke)>0){no_joke[names(no_joke)[which.max(no_joke)]]} else {key <- c("J"=0)} #get the most-occurring non-Joker
  
  hand_rank <- sum(13**table(str_replace(split, names(key), "J"))) #replace the most-occuring card with a "J"
  hand_score <- lapply(split, match, c("J", "2", "3", "4", "5", "6", "7", "8", "9", "T", "Q", "K", "A"))
  
  unlist(c(hand_rank, hand_score))
}

day7_1 <- function(input){
  data.frame(input) |> 
    separate(input, into = c("hand", "value"), sep = " ") |> 
    rowwise() |> 
    mutate(scores = list(hand_value(hand))) |> 
    ungroup() |> 
    unnest_wider(scores,names_sep = "card") |> 
    arrange(across(starts_with("scorescard"))) |> 
    mutate(n = row_number(),
           a = n*as.numeric(value)) |> 
    summarise(sum(a))
}

#answer
day7_1(input)


day7_2 <- function(input){
  data.frame(input) |> 
    separate(input, into = c("hand", "value"), sep = " ") |> 
    rowwise() |> 
    mutate(scores = list(hand_value2(hand))) |> 
    ungroup() |> 
    unnest_wider(scores,names_sep = "card") |> 
    arrange(across(starts_with("scorescard"))) |> 
    mutate(n = row_number(),
           a = n*as.numeric(value)) |> 
    summarise(sum(a))
}

day7_2(input)
