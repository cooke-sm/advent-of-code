test <- readLines("~/advent-of-code/2022/inputs/day1_test")
input <- readLines("~/advent-of-code/2022/inputs/day1.txt")


max(unlist(lapply(split(test, cumsum(test == "")), \(x) sum(as.numeric(x), na.rm = TRUE))))
max(unlist(lapply(split(input, cumsum(input == "")), \(x) sum(as.numeric(x), na.rm = TRUE))))




