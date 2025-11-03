test <- readLines("~/advent-of-code/2024/data/day13_test")


frob <- function(x,y){
  ((x*y)-x)-y
}

frob(86,37)
