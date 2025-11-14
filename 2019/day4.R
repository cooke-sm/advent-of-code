input <- "278384-824795"
test <- c(111111, 223450, 123789)


checker <- function(n_in, part = 1){
  n <- as.numeric(strsplit(as.character(n_in), "")[[1]])
  crit1 <- TRUE
  crit2 <- FALSE
  crit3 <- FALSE

  for(i in seq(2, length(n))){
    
    current_n <- TRUE

    if(n[i-1] > n[i]){
      crit1 <- FALSE
    }
    if(i > 1 & i < 6){
      l <- n[i-1]
      r <- n[i+1]

      if(n[i] == l & n[i] == r){
        crit2 <- FALSE
        current_n <- FALSE
      }

      if((n[i] == l | (n[i] == r & n[i] != r2  ) & current_n){
        crit2 <- TRUE
        crit3 <- TRUE
      }   

      }
      
    }
  
  
  if(crit1 & crit2){
    n_in    
  } else {
    NA
  }
}



day4_1 <- function(input){
  x_y <- as.numeric(strsplit(input, "-")[[1]])
  range <- seq(x_y[1], x_y[2])

  checked <- unlist(lapply(range, checker))

  length(checked[!is.na(checked)])

 
}


day4_1(input)
checker(223444)



  