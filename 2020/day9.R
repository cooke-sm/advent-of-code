
test_input <- c(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576)

input <- as.numeric(readLines("~/advent-of-code/2020/inputs/day9.txt"))

day9_1 <- function(input, win_len){
  for(i in seq(win_len + 1, length(input))){
    
    to_check <- input[i]
    check_against <- input[(i-(win_len+1)):(i-1)]
    
    cont <- check_against %in% abs(check_against - to_check)
    
    if(!any(cont)){
      print(to_check)
      break
    }
    
  }
}

day9_1(test_input,5)

#127

day9_1(input, 25)


#check for contiguous numbers

day9_2 <- function(input, win_len){
  for(i in seq(win_len + 1, length(input))){
    
    to_check <- input[i]
    check_against <- input[(i-(win_len+1)):(i-1)]
    
    cont <- check_against %in% abs(check_against - to_check)
    
    if(!any(cont)){
      answer <- to_check
      break
    }
    
  }
  
  for(i in seq(1,length(input))){
    
    output <- NULL
    test <- TRUE
    new_answer <- NULL
    
    p <- i
    
    while(test){
      
      output[length(output)+1] <- input[p]
      p <- p+1
      
      if(sum(output) > answer){
        test <- FALSE
      } else if(sum(output) == answer) {
        new_answer <- output
        test <- FALSE
      }
    }
    
    if(!is.null(new_answer)){break}
  }
  
  
  min(new_answer)+max(new_answer)
  
  
}

day9_2(input, 25)
