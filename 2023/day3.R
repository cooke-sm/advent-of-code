library(stringr)
test <- readLines("~/advent-of-code/2023/data/day3_input.txt")

#make an array of the input
input <- t(array(unlist(str_split(test, "")), dim = c(length(test),nchar(test[1]))))

#a function to use later
symbol_checker <- function(row,col,array_input){
  check = FALSE
  grid <- expand.grid(c((row-1):(row+1)),c((col-1):(col+1)))
  
  for(i in seq(1:9)){
    
    if(dplyr::between(grid[i,1],1,nrow(array_input)) 
       & dplyr::between(grid[i,2],1,ncol(array_input))){
      
      if(str_detect(array_input[grid[i,1],grid[i,2]], "[0-9\\.]")){
      } else {check = TRUE}
    }
    
  }
  #return true/false
  check
}

day3_1 <- function(array_input){
  current_number <- vector(mode = "character")
  output <- vector(mode = "numeric")
  to_include <- FALSE
  
  for(row in seq(1,nrow(array_input))){
    for(cell in seq(1,ncol(array_input))){
      
      contents <- array_input[row,cell]
      
      if(str_detect(contents, "[0-9]")){
        current_number[length(current_number)+1] <- contents
        if(to_include == FALSE){
          to_include <- symbol_checker(row, cell, array_input)
          }
        
      } else {
        if(to_include){
          output[length(output)+1] <- as.numeric(str_c(current_number, collapse = ""))}
        
          current_number <- vector(mode = "character")
          to_include <- FALSE
      }
      }
  }
  sum(output, na.rm = TRUE)
}

#answer
day3_1(input)

symbol_checker2 <- function(row,col,array_input){
  
  count <- 0
  locs <-  vector(mode='numeric')
  grid <- expand.grid(c((row-1):(row+1)),c((col-1):(col+1)))
  
  for(i in seq(1:9)){
    
    if(dplyr::between(grid[i,1],1,nrow(array_input)) 
       & dplyr::between(grid[i,2],1,ncol(array_input))){
      
      if(str_detect(array_input[grid[i,1],grid[i,2]], "[0-9]")){
        
        count <- count+1
        print(count)
        
      } else {
      }
      
    }
    #return true/false
  
  }
  count
}


day3_2 <- function(array_input){
  current_number <- vector(mode = "character")
  output <- vector(mode = "numeric")
  to_include <- FALSE
  
  for(row in seq(1,nrow(array_input))){
    for(cell in seq(1,ncol(array_input))){
      
      contents <- array_input[row,cell]
      
      if(str_detect(contents, "^\\*$")){
        
        output[length(output)+1] <- symbol_checker2(row, cell, array_input)
        
      }
    
    }
  }
  output
}

day3_2(input)


