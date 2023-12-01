test_input <- c(
  "1-3 a: abcde",
  "1-3 b: cdefg",
  "2-9 c: ccccccccc"
)


day2_1 <- function(input) {
  split <- strsplit(input, ":")
  crit <- strsplit(split[[1]][1], " ")
  pword <- strsplit(split[[1]][2], "")[[1]]

  bounds <- as.numeric(strsplit(crit[[1]][1], "-")[[1]])

  dplyr::between(length(pword[pword == crit[[1]][2]]), bounds[1], bounds[2])
}


answer <- sapply(test_input, day2_1)

length(answer[answer == TRUE])

input <- readLines("~/advent-of-code/2020/inputs/day2.txt")

answer <- sapply(input, day2_1)

length(answer[answer == TRUE])

day2_2 <- function(input) {
  split <- strsplit(input, ":")
  crit <- strsplit(split[[1]][1], " ")
  pword <- strsplit(trimws(split[[1]][2]), "")[[1]]

  bounds <- as.numeric(strsplit(crit[[1]][1], "-")[[1]])

  xor(
    (pword[bounds[1]] == crit[[1]][2]),
    (pword[bounds[2]] == crit[[1]][2])
  )
}


answer2 <- sapply(input, day2_2)

length(answer2[answer2 == TRUE])
