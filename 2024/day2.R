test <- readLines("~/advent-of-code/2024/data/day2_test")
input <- readLines("~/advent-of-code/2024/data/day2", warn = FALSE)



comparer <- function(report){
  diffs <- report-c(report[-1],0)
  
  diffs <- diffs[-length(diffs)]
  
  if( all(dplyr::between(diffs, 1, 3)) | all(dplyr::between(diffs, -3, -1))){
    TRUE
  } else {
    FALSE
  }
  
}

day2_1 <- function(input){
  
  parsed <- lapply(strsplit(input, "\\s+"), as.numeric)
  
  sum(unlist(lapply(parsed, comparer)))
  
}

day2_1(test)
#2

day2_1(input)
#224


comparer2 <- function(report){
  diffs <- report-c(report[-1],0)
  
  diffs <- diffs[-length(diffs)]
  
  if( all(dplyr::between(diffs, 1, 3)) | all(dplyr::between(diffs, -3, -1))){
    TRUE
  } else {
    
    #bruteforce
    
    output <- list()
    
    for(i in seq(1,length(report))){
      output[i] <- comparer(report[-i])
    }
    any(unlist(output)) 
  }
  
}


day2_2 <- function(input){
  
  parsed <- lapply(strsplit(input, "\\s+"), as.numeric)
  
  sum(unlist(lapply(parsed, comparer2)))
  
}

day2_2(test)
#4

day2_2(input)
#293

