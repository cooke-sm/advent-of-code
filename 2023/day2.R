library(tidyverse)

input <- readLines("~/advent-of-code/2023/data/day2_input.txt")

day2_parse <- function(input_line){
  step1 <- str_split(input_line, ":")[[1]]
  game_no <- parse_number(step1[1])
  
  step2 <- str_split(step1[2], ";")[[1]]
  
  output <- list()
  
  for(each in seq(1,length(step2))){
    output[[each]] <- c("game" = game_no,
         "round_no" = each,
         "r" = str_extract(step2[each], "\\d+ red"),
         "g" = str_extract(step2[each], "\\d+ green"),
         "b" = str_extract(step2[each], "\\d+ blue"))
  }
  bind_rows(output) |> 
    mutate(across(c(1:5),parse_number))
}


day2_1 <- function(r_lim,g_lim,b_lim, input){
  
  reduce(lapply(input, day2_parse), bind_rows) |>
    group_by(game) |> 
    summarise(r = max(r, na.rm = TRUE),
              g = max(g, na.rm = TRUE),
              b = max(b, na.rm = TRUE)) |>
    rowwise() |> 
    mutate(valid = all(r <= r_lim,
                       g <= g_lim,
                       b <= b_lim)) |> 
    ungroup() |> 
    filter(valid == TRUE) |> 
    summarise(answer = sum(game))
  
}

#answer
day2_1(12,13,14, input)

day2_2 <- function(input){
  
  reduce(lapply(input, day2_parse), bind_rows) |>
    group_by(game) |> 
    summarise(r = max(r, na.rm = TRUE),
              g = max(g, na.rm = TRUE),
              b = max(b, na.rm = TRUE)) |>
    mutate(power = r*g*b) |> 
    summarise(sum(power))
  
}

#answer
day2_2(input)



                  