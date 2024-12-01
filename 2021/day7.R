test <- readLines("~/advent-of-code/2021/data/day7_test")
input <- readLines("~/advent-of-code/2021/data/day7")


day7_1 <- function(input){
  crabs <- as.integer(strsplit(input, ",")[[1]])
  
  pos <- seq(min(crabs),max(crabs)) #can optimise here
  
  min(unlist(lapply(pos, \(x) sum(abs(crabs-x)))))

}

day7_1(test)


sumfuel <- function(start,stop){
  #gaussian distance func

  n <- abs(stop-start)
  (n)*((n+1)/2)

}

day7_2 <- function(input){
  crabs <- as.integer(strsplit(input, ",")[[1]])
  
  pos <- seq(min(crabs),max(crabs)) #can optimise here
  
  min(unlist(lapply(pos, \(x) sum(sumfuel(x,crabs))))) #only change here to compute gaussian distance
  
}

day7_2(input)

