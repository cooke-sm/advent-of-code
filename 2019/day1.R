input <- readLines("~/advent-of-code/2019/data/day1_input.txt")

mafs <- function(x){
  (as.numeric(x)%/%3)-2
}

day1_1 <- function(input){

  Reduce("+", lapply(input, mafs))

}

day1_1(input)

logic <- function(y){

  output <- vector(mode = "numeric")
  mass <- mafs(y)
  output[length(output)+1] <- mass

  while(mass > 0){
    mass <- mafs(mass)

    if(mass > 0){output[length(output)+1] <- mass
    }

  }

  sum(output)
}

day1_2 <- function(input){

  Reduce("+", lapply(input, logic))

}

day1_2(input)

