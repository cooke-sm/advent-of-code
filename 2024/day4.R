test <- readLines("~/advent-of-code/2024/data/day4_test")
input <- readLines("~/advent-of-code/2024/data/day4")


checker <- function(index, xmat){
  n <- nrow(xmat)
  output <- list()
  output[1] <- paste(xmat[index,], collapse = "")
  
  
  if(index > 3 & index < n){
  output[2] <- paste(diag(xmat[((n+1)-index):n, 1:(0+index)]), collapse = "")
  output[3] <- paste(diag(xmat[1:(0+index),((n+1)-index):n]), collapse = "")
  
  } else if (index == n){
    output[2] <- paste(diag(xmat[((n+1)-index):n, 1:(0+index)]), collapse = "")
  }
  output
}

day4_1 <- function(input){
  
  xmat <- do.call(rbind, (strsplit(input, "")))
  horizontal <- lapply(seq(1,nrow(xmat)), checker, xmat)
  rotated <- t(xmat)[, ncol(xmat):1]
  vertical <- lapply(seq(1, nrow(rotated)), checker, rotated)
  
  sum(stringr::str_count(unlist(list(horizontal, vertical)), "XMAS"),
      stringr::str_count(unlist(list(horizontal, vertical)), "SAMX"))
  
}

day4_1(test)
#18

day4_1(input)
#2599

checker2 <- function(matrix){
  d1 <- diag(matrix)
  if(all(d1 == c("M","A","S")) | all(d1 == c("S","A","M"))){
    d2 <- diag(t(matrix)[, ncol(matrix):1])
    all(d2 == c("M","A","S")) | all(d2 == c("S","A","M"))
  } else {
    FALSE
    }
}

day4_2 <- function(input){
  
  xmat <- do.call(rbind, (strsplit(input, "")))
  n <- nrow(xmat) #the input is a square
  
  output <- list()
  
  for(row in seq(1, n-2)){
    for(col in seq(1, n-2)){
      output[[length(output)+1]] <- xmat[row:(row+2), col:(col+2)]
      
    }
  }
  sum(unlist(lapply(output, checker2)))
  
}

day4_2(input)
#1948
