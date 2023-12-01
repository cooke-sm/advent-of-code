input <- readLines("~/advent-of-code/2023/data/day1_input.txt")
numwords <- c("one" = "1","two" = "2","three" = "3","four" = "4","five" = "5","six" = "6","seven" = "7","eight" = "8","nine" = "9")
num_regex <- c("one|two|three|four|five|six|seven|eight|nine")



day1_1 <- function(line){
  nums <-  stringr::str_extract_all(line, "[0-9]")[[1]]
  as.numeric(stringr::str_c(nums[1],nums[length(nums)],collapse = ""))
  
}
#answer1
sum(unlist(lapply(input, day1_1)))

day1_2 <- function(line){
  #could use str_extract rather than str_extract_all here. str_extract_all doesn't work because some matches overlap
  first <- stringr::str_extract_all(line, stringr::str_c("(\\d|", num_regex,")"))[[1]][1]
  #bit of a hack - flip it around and take the first match
  last <- stringi::stri_reverse(stringr::str_extract_all(stringi::stri_reverse(line), stringr::str_c("(\\d|", stringi::stri_reverse(num_regex),")"))[[1]][1])
  nums <- stringr::str_replace_all(c(first, last), numwords)
  as.numeric(stringr::str_c(nums[1],nums[length(nums)],collapse = ""))
}

#answer2
sum(unlist(lapply(input, day1_2)))


