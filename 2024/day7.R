test <- readLines("~/advent-of-code/2024/data/day7_test")
input <- readLines("~/advent-of-code/2024/data/day7")

day7_1 <- function(input){
  df <- data.frame(do.call(rbind, strsplit(input, ": ")))
  
  df$X2 <- lapply(df$X2, \(x) as.numeric(unlist(strsplit(x, " "))))
  
  output <- vector(mode = 'logical')
  
  for(row in seq(1,nrow(df))){
    
    target <- as.numeric(df[row,1])
    output[row] <- target %in% recur_tree(unlist(df[row,2]), target)
  }
  
  sum(as.numeric(df[output == TRUE,]$X1))
}

recur_tree <- function(vector, target){
  #takes the vector and calls itself for each of the paths on the tree
  
  if(vector[1] > target){  #exit condition
    vector
  }
  
  else {
    
    if(length(vector) == 1) {
      vector
    } else {
      add <- vector[1]+vector[2]
      mult <- vector[1]*vector[2]
      
      addv <- vector[2:length(vector)]
      multv <- vector[2:length(vector)]
      
      addv[1] <- add
      multv[1] <- mult
      
      c(recur_tree(addv, target), recur_tree(multv, target))
    }
  }
  
}


day7_1(test)
#3749

format(day7_1(input), scientific = FALSE)

recur_tree2 <- function(vector, target){
  #takes the vector and calls itself for each of the paths on the tree
  
  if(vector[1] > target){ #exit condition
    vector
  }
  
  else {
    
    if(length(vector) == 1) { 
      vector
    } else {
      add <- vector[1]+vector[2]
      mult <- vector[1]*vector[2]
      comb <- as.numeric(paste0(vector[1], vector[2]))
      
      addv <- vector[2:length(vector)]
      multv <- vector[2:length(vector)]
      combv <- vector[2:length(vector)]
      
      addv[1] <- add
      multv[1] <- mult
      combv[1] <- comb
      
      
      c(recur_tree2(addv, target), recur_tree2(multv, target), recur_tree2(combv, target))
    }
  }
}

day7_2 <- function(input){
  df <- data.frame(do.call(rbind, strsplit(input, ": ")))
  
  df$X2 <- lapply(df$X2, \(x) as.numeric(unlist(strsplit(x, " "))))
  
  output <- vector(mode = 'logical')
  
  for(row in seq(1,nrow(df))){
    
    target <- as.numeric(df[row,1])
    output[row] <- target %in% recur_tree2(unlist(df[row,2]), target)
  }
  
  sum(as.numeric(df[output == TRUE,]$X1))
}

day7_2(test)
#11387

format(day7_2(input), scientific = FALSE)
#492383931650959




