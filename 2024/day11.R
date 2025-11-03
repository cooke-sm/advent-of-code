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
    starttime <- Sys.time()
    stones <- lapply(stones, \(x) unlist(lapply(x, rules)))
    endtime <- Sys.time()
    print(paste("iteration:", blink,"done.","Time taken:",endtime - starttime))
    
  }
  
  stones
}

day11_1(test)
#55312

day11_1(input)

# Recursive function
recursion <- function(stone, n, limit) {
  n <- n + 1
  
  if (n > limit) {
    count[[as.character(stone)]][[as.character(n)]] <<- c(count[[as.character(stone)]][[as.character(n)]], 1) 
    return(1)
  } else {
    if (!is.null(map[[as.character(stone)]][[as.character(n)]])) {
      print(paste("hits:", stone, "at:", n))
      count[[as.character(stone)]][[as.character(n)]] <<- c(count[[as.character(stone)]][[as.character(n)]], map[[as.character(stone)]][[as.character(n)]])
    } else {
      if (stone == 0) {
        stone_children <- recursion(1, n, limit)
      } else { 
        if (nchar(stone) %% 2 == 0) {
          stone_children <- c(
            recursion(as.numeric(substring(stone, 1, nchar(stone) / 2)), n, limit),
            recursion(as.numeric(substring(stone, (nchar(stone) / 2) + 1, nchar(stone))), n, limit)
          )
        } else {
          stone_children <- recursion(stone * 2024, n, limit)
        }
      }
      map[[as.character(stone)]][[as.character(n)]] <<- sum(stone_children)
    }
  }
}

# Main function
day11_2 <- function(input, limit = 25) {
  stones <- as.numeric(strsplit(input, " ")[[1]])
  
  map <<- list()
  count <<- list()
  n <- -1
  
  recursion(stones, n, limit)
  
  sum(unlist(count))
}

# Example Input
input <- "0 1 2 3"
result <- day11_2(input)
print(result)


day11_2(test, 2)
length(unlist(day11_1(test, 11)))


count[[as.character(1)]]
