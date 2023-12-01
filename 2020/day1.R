

day1_part1 <- function(input){
  sub <- abs(input-2020)
  answer <- intersect(input, sub)
  
  if((sum(answer) == 2020) 
     & 
     (length(answer) == 2)){
    prod(answer)
  } else if(sum(answer == 2020)
            &
            (length(answer)>2)){
    print("Longer than 2")
  } else{
    print("No sum")}
}

test_input <- c(1721, 979, 366, 299, 675, 1456)

day1_part1(test_input)

input <- as.numeric(readLines("~/advent-of-code/2020/inputs/day1.txt"))

day1_part1(input)

input2 <- sort(input)

day1_part2 <- function(input){
  grid <- expand.grid(input, input, input)
  grid$sums <- rowSums(grid)
  sums <- grid[grid$sums == 2020,]
  prod(sums[1,1:3])
}

day1_part2(input)


