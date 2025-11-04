input <- readLines("~/advent-of-code/2019/data/day2_input.txt")


day2_1 <- function(input, noun, verb){

  instr <- as.numeric(strsplit(input, ",")[[1]])

  instr[2] <- noun
  instr[3] <- verb

  halt <- FALSE
  pos <- 1 #remember that it's zero-indexed

  while(!halt){

    x <- instr[(instr[pos+1]+1)]
    y <- instr[(instr[pos+2]+1)]

    if(instr[pos] == 1){

      instr[instr[pos+3]+1] <- x+y

      pos <- pos+4
      
    } else if(instr[pos] == 2){
      instr[instr[pos+3]+1] <- x*y

      pos <- pos+4

    } else if(instr[pos] == 99){
      halt <- TRUE

    } else {

      print ("ERROR!")
      break

    }
 
  }
  instr[1]
}

day2_1(input, 12,2)


day2_2 <- function(input){

  combo <- expand.grid(n = 0:99, v = 0:99)

  combo_list <- split(combo, seq(nrow(combo)))

  output <- lapply(combo_list, \(x) day2_1(input, x[[1]],x[[2]]))

  ans <- combo_list[which(sapply(output, \(x) x == 19690720))]

  100 *ans[[1]][1] + ans[[1]][2]


}

day2_2(input = input)




