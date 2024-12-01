test <- readLines("~/advent-of-code/2021/data/day9_test")
input <- readLines("~/advent-of-code/2021/data/day9")

seeker <- function(row,col, grid){
  surrounds <- c(grid[row,col+1],
                 grid[row,col-1],
                 grid[row-1,col],
                 grid[row+1,col])
  
  if(any((surrounds <= grid[row,col]))){
    NA
  } else {
    grid[row,col]
  }
}

day9_1 <- function(input){
  
  #parse the grid and wrap it in boundary to save the seeker going out of bounds
  grid <- do.call(rbind, lapply(strsplit(input, ""), as.numeric))
  mat <- matrix(c(9), nrow = nrow(grid)+2, ncol = ncol(grid)+2)
  mat[2:(nrow(grid)+1),2:(ncol(grid)+1)] <- grid
  
  ans <- c()
  
  for(row in seq(2,nrow(mat)-1)){
    
    for(col in seq(2, ncol(mat)-1)){
      
      ans[length(ans)+1] <- seeker(row,col,mat)
    }
  }
  
  sum(ans[!is.na(ans)]+1)
}



day9_1(test)
#15

day9_1(input)

