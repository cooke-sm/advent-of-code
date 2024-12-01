test <- readLines("~/advent-of-code/2021/data/day3_test")
input <- readLines("~/advent-of-code/2021/data/day3")


bin_to_dec <- function(bin_input){
  bitvals = vector(mode = 'numeric')
  for(i in seq(1,length(bin_input))){
    bitvals[i] <- (2**(i-1))
    
  }
  sum(rev(bin_input)*bitvals)
}


day3_1 <- function(input){
  input_list <- strsplit(input, "")
  bin_array <- Reduce(rbind,lapply(input_list, as.numeric))
  col_sums <- colSums(bin_array)
  
  gamma <- (col_sums-(nrow(bin_array)/2))>0 #this may cause an error if there are 0s
  epsilon <- 1-gamma
  
  bin_to_dec(gamma)*bin_to_dec(epsilon)
  
}

day3_1(test)
#198

day3_1(input)
#2972336


selecter1 <- function(x, col){
  
  if(is.null(nrow(x))){
    
    x
    
  } else {
    
    y <- as.logical(x[,col])
    
    if(sum(y)<(nrow(x)/2)){
      y <- !y
    } 
    selecter1(x[y,], col+1)
  }
  
}

selecter2 <- function(x, col){
  
  if(is.null(nrow(x))){
    
    x
    
  } else {
    
    y <- !as.logical(x[,col])
    
    if(sum(y)>(nrow(x)/2)){
      y <- !y
    } 
    
    selecter2(x[y,], col+1)
  }
  
  
}


day3_2 <- function(input){
  input_list <- strsplit(input, "")
  bin_array <- Reduce(rbind,lapply(input_list, as.numeric))
  
  ogr <- selecter1(bin_array, 1)
  csr <- selecter2(bin_array, 1)
  
  bin_to_dec(ogr)*bin_to_dec(csr)
}

day3_2(input)
#3368358

