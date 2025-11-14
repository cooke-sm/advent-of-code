test <- readLines("~/advent-of-code/2019/data/day3_test.txt")
input <- readLines("~/advent-of-code/2019/data/day3_input.txt")
library(stringr)


mapper <- function(instruction){
  path <- c("lat" = 0, "long" = 0)
  origin <- path

  for(item in seq(1,length(instruction))){
    
    distance <- as.numeric(str_extract(instruction[item], "\\d+"))
    direction <- str_extract(instruction[item], "[A-Z]")
    
    switch(direction,
      U = origin["long"] <- origin["long"] + distance, 
      D = origin["long"] <- origin["long"] + (distance*-1),
      R = origin["lat"] <- origin["lat"] + distance, 
      L = origin["lat"] <- origin["lat"] + (distance *-1),
    )

    path <- rbind(path, origin)
    
  }
  

  for(line in seq(2,length(path))){

    print(path[(line-1):line])

    

  }
  path
}

day3_1 <- function(input){

  #parse
  strsplit(input, ",")

}

x <- day3_1(test)


mapper(x[[1]])
