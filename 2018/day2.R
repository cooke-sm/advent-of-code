input <- readLines("~/advent-of-code/2018/data/day2_input.txt")

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


