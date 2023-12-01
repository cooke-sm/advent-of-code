library(tidyverse)
library(igraph)

test <- read_lines("~/advent-of-code/2020/inputs/day7_test.txt")


parser <- function(to_parse){
  
  to_parse <- unlist(to_parse)
  
  if(to_parse[2] == "no other bags."){
    
  } else{
  
  parent <- to_parse[1]
  
  children <- sapply(to_parse[-1], \(x) rep(trimws(str_extract(x, "[^0-9\\.]+"),"left"), parse_number(str_extract(x, "[0-9]"))),USE.NAMES = FALSE) |> unlist()
  
  tibble(parent, children)
  }
  
}

parse_graph <- function(input){
  
  to_parse <- lapply(str_split(input, " contain "), str_split, ", ")
  
  lapply(to_parse, parser) |> bind_rows()

}



graph_from_edgelist(as.matrix(parse_graph(test))) |> plot()









