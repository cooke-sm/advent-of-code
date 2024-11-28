test <- readLines("~/advent-of-code/2021/data/day5_test")
input <- readLines("~/advent-of-code/2021/data/day5")


diag_remover <- function(coords){
  x <- unlist(coords)
  x <- x[1:2]-x[3:4]
  any(x == 0)
}

line_reader <- function(coords){
  #take the start/finish coords and return a list of all the places where the line visits
  coords <- unlist(coords)
  dif <- coords[1:2] - coords[3:4]
  
  if(dif[1] == 0){
    #y coord
    output <- stringr::str_c(coords[1], ", ", seq(coords[2], coords[4]))
  } else {
    #x coord
    output <- stringr::str_c(seq(coords[1], coords[3]), ", ", coords[2])
  }
  output
}

diag_reader <- function(coords){
  #same as the above with no logic
  coords <- unlist(coords)
  
  output <- stringr::str_c(seq(coords[1], coords[3]), ", ", seq(coords[2],coords[4]))
  
  output
}

day5_1 <- function(input){
  
  dict <- list()
  
  #read the data in
  lines <- lapply(strsplit(input, " -> "), \(x) strsplit(x, ","))
  lines <- lapply(lines, \(x) lapply(x, as.numeric))
  straight_lines <- lines[unlist(lapply(lines, diag_remover))]
  
  #returns a vect of where the lines pass through 
  locs <- unlist(lapply(straight_lines, line_reader))
  
  #how many are hit by 2 or more
  length(table(locs)[table(locs)>1])
}

day5_1(test)
#5

day5_1(input)
#5124


day5_2 <- function(input){
  
  #same as the above but take the complement of the straight lines as diagonals
  
  dict <- list()
  
  #read the data in
  lines <- lapply(strsplit(input, " -> "), \(x) strsplit(x, ","))
  lines <- lapply(lines, \(x) lapply(x, as.numeric))
  straight_lines <- lines[unlist(lapply(lines, diag_remover))]
  diagonals <- setdiff(lines, straight_lines)
  
  #returns a vect of where the lines pass through 
  locs <- unlist(list(lapply(straight_lines, line_reader),lapply(diagonals, diag_reader)))
  
  #how many are hit by 2 or more
  length(table(locs)[table(locs)>1])
  
}

day5_2(test)
#12

day5_2(input)
#19771