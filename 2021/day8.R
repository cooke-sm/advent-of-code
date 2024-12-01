library(stringr)

test <- readLines("~/advent-of-code/2021/data/day8_test")
input <- readLines("~/advent-of-code/2021/data/day8")

#4 = 4, 1 = 2, 7 = 3, 8 = 7 <- digits :)

day8_1 <- function(input){
  output <- str_split(str_extract(input, "(?<=\\| ).+"), " ")
  
  counts <- table(nchar(unlist(output)))
  
  sum(counts[c(1,2,3,6)])
}

day8_1(test)
#26

day8_1(input)
#362


replacer <- function(input){
  #takes a line as an input, returns the value of the output
  dict <- factor(x = c(0:9), labels = c(0:9))
  
  digits <- str_split(str_extract(input, ".+(?= \\| )"), " ")[[1]]
  nums <- str_split(str_extract(input, "(?<=\\| ).+"), " ")

  refs <- digits[nchar(digits) %in% c(2,3,4)]
  refs <- refs[order(nchar(refs))]

  
  assoc <- lapply(setdiff(digits, refs), \(x) str_compare(x,refs[1],refs[2],refs[3]))
  names(assoc) <- setdiff(digits, refs)
  
  assoc
}

replacer(test[1])

str_compare <- function(input,x,y,z){
  #takes two strings, compares them for common characters
  input <- str_split(input, "")[[1]]
  x <- str_split(x, "")[[1]]
  y <- str_split(y, "")[[1]]
  z <- str_split(z, "")[[1]]
  
  list(length(intersect(input,x)),
    length(intersect(input,y)),
    length(intersect(input,z)))
}




day8_2 <- function(input){
  
  #just lapply the input into replacer()
  
  
}



