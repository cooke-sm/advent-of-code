test <- readLines("~/advent-of-code/2024/data/day8_test")
input <- readLines("~/advent-of-code/2024/data/day8")

antinode <- function(pcoords,qcoords){
  rbind(pcoords + (pcoords-qcoords) * -2, 
    qcoords + (qcoords - pcoords) * -2)
}

day8_1 <- function(input){
  
  raw <- strsplit(input, "")
  mat <- do.call(rbind, raw)
  values <-   unique(unlist(raw))
  
  output <- data.frame(row.names = c("x","y"))
  
  for(value in values[values != "."]){
    coords <- which(mat == value, arr.ind = TRUE)
    combinations <- (t(combn(1:nrow(coords), 2))) # col 1 is p, 2 is q
    
    anodes <- do.call(rbind, lapply(seq(1:nrow(combinations)),
                     \(x) antinode(coords[combinations[x,1],], 
                                   coords[combinations[x,2],])))
    output <- rbind(output, anodes)
  }
  nrow(unique(subset(output, row <= nrow(mat) & col <= ncol(mat) & row > 0 & col > 0)))
}

day8_1(test)
#14

day8_1(input)
#311

recur_node <- function(pcoords, qcoords, times){
  
  # takes the two points and expands the pcoords argument out
  # returns a matrix of all the points
  # I enjoyed this one <3
  
  if(times > 0){
    times <- times -1
    diff <- (pcoords-qcoords) * -1
    rbind(qcoords,
          recur_node(pcoords+diff,qcoords+diff, times))
  } else (
    rbind(qcoords)
  )
}

day8_2 <- function(input){
  
  raw <- strsplit(input, "")
  mat <- do.call(rbind, raw)
  values <-   unique(unlist(raw))
  
  output <- data.frame(row.names = c("x","y"))
  
  for(value in values[values != "."]){
    coords <- which(mat == value, arr.ind = TRUE)
    combinations <- (t(combn(1:nrow(coords), 2))) # col 1 is p, 2 is q
    
    anodes <- do.call(rbind, lapply(seq(1:nrow(combinations)),
                                   \(x) rbind(recur_node(coords[combinations[x,1],],
                                                         coords[combinations[x,2],], nrow(mat)), #can be optimised here
                                              recur_node(coords[combinations[x,2],],
                                                         coords[combinations[x,1],], nrow(mat))
                                                 )))
    output <- rbind(output, anodes)
  }
  nrow(unique(subset(output, row <= nrow(mat) & col <= ncol(mat) & row > 0 & col > 0)))
}

day8_2(test)
#34

day8_2(input)
#1115