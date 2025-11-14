input <- readLines("~/advent-of-code/2018/data/day2_input.txt")

comparer <- function(x,y){
  difs <- 0

  for(letter in seq(1,length(x))){
    
    if((x[letter] == y[letter]) & (difs < 2)){
      
    } else if(difs >1){
      break
    } else{
      difs <- difs +1
    }
        
  }

  if(difs == 1){
    intersect(x, y)
  } else {}
}


day2_1 <- function(input){

  codes <- strsplit(input, "")
  tables <- lapply(codes, table)
  output <- c("2" = 0, "3" = 0)

  for(tab in tables){
    if(any(tab == 2)){
      output["2"] <- output["2"]+1
    }
    if(any(tab == 3)){
      output["3"] <- output["3"]+1
    }

  }
  output[1]*output[2]
}

day2_1(input)

day2_2 <- function(input){
  codes <- strsplit(input, "")

  for(code in seq(1, length(codes))){

    matches <- unlist(lapply(codes, \(x) comparer(codes[[code]], x)))
    
    if(length(matches) > 0){
      print((paste(matches, collapse = "")))
      print(codes[[code]])
    }
  }

}

day2_2(input = input)
