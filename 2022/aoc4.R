#day 4

input <- readLines(con = "~/advent-of-code/2022/inputs/day4.txt")

#this is just a function that takes the input string "4-5" and evaluates it as a vector c(4:5)
interp <- function(string){
  string <- sub("-",":",x=string)
  eval(parse(text=paste("c(",string,")")))
}

#use set theory to check if all elements of the two lists are comparable
aoc4 <- function(input){
  input <- strsplit(input,",")
  
  subset_pairs = 0
  
  for(i in seq_along(input)){
    
    elf_pairs <- input[i][[1]]
    elf1 <- interp(elf_pairs[1])
    elf2 <- interp(elf_pairs[2])
    if(all(is.element(elf1, elf2)) | all(is.element(elf2, elf1))){
      subset_pairs <- subset_pairs+1
    }
  }
  return(subset_pairs)
}


aoc4(input)

#Part 2

#Just change "all" to "any"

aoc4_pt2 <- function(input){
  input <- strsplit(input,",")
  
  subset_pairs = 0
  
  for(i in seq_along(input)){
    
    elf_pairs <- input[i][[1]]
    elf1 <- interp(elf_pairs[1])
    elf2 <- interp(elf_pairs[2])
    if(any(is.element(elf1, elf2)) | any(is.element(elf2, elf1))){
      subset_pairs <- subset_pairs+1
    }
  }
  return(subset_pairs)
}


aoc4_pt2(input)

