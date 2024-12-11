test <- readLines("~/advent-of-code/2024/data/day11_test")
input <- readLines("~/advent-of-code/2024/data/day11")

rules <- function(stone){
  
  if(as.numeric(stone) == 0){
    
    #rule 1
    stone <- "1"
    
  } else if(nchar(stone)%%2 == 0){
    
    stone <- as.character(c(as.numeric(substring(stone, 1,(nchar(stone)/2))),
               as.numeric(substring(stone, (nchar(stone)/2)+1, nchar(stone)))))
    
  } else {
    
    stone <- as.character(as.numeric(stone)*2024)
  }
  
  stone
}

day11_1 <- function(input, times = 25){
  
  stones <- as.list(strsplit(input, " ")[[1]])
  
  for(blink in seq(1,times)){
    stones <- lapply(stones, \(x) unlist(lapply(x, rules)))
    
  }
  
  length(unlist(stones))
}

day11_1(test)
#55312

day11_1(input)




