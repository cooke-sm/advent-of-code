library(tidyverse)


test <- readLines("~/advent-of-code/2023/data/day15_test.txt")
input <- readLines("~/advent-of-code/2023/data/day15_input.txt")

algo <- function(current, letter) {
  asky <- as.integer(charToRaw(letter))

  ((current + asky) * 17) %% 256
}

day15_1 <- function(input) {
  results <- lapply(str_split(input, ",")[[1]], \(x) Reduce(algo, str_split(x, "")[[1]], 0))

  sum(unlist(results))
}


day15_1(input)


lens_logic <- function(lens, box_n) {
  label <- str_extract(lens, "[aA-zZ]+")
  lens_strength <- as.numeric(str_extract(lens, "\\d+"))
  
  
  label_in_box <- length(which(names(box_n) == label))>0
  
  
  if (str_detect(lens, "-")) {
    
    if(label_in_box){box_n <- box_n[-which(names(box_n) == label)]} 
    
    
  }
  
  else{
    
    if (label_in_box) {
      box_n[which(names(box_n) == label)] <- lens_strength
    } else {
      box_n <- setNames(c(box_n, lens_strength), c(names(box_n), label))
    }
  }
  
  if(is.null(box_n))(box_n <- c("123" = 0))
    
  box_n
}


focus_power <- function(box, input) {
  # input is the list of boxes
  # box number is the current box
  if (is.null(box)) {} else {
    output <- vector("numeric")

    for (lens in seq(1, length(box))) {
      output[lens] <- match(list(box), input) * box[lens] * lens
    }

    output
  }
}


day15_2 <- function(input) {
  boxes <- vector(mode = "list", length = 256)
  
  
  hashtable <- unnest(as.tibble(do.call(cbind,
                                        list("x1" = str_split(input, ",")[[1]], 
                                             "x2" = lapply(str_split(input, ",")[[1]], \(x) Reduce(algo, str_split(str_extract(x,pattern = "[aA-zZ]+"), "")[[1]], 0)
                                             )))
  ), cols = c(x1,x2))
  
  
  # do the mapping here
  for(hash in seq(1, nrow(hashtable))) {
    box <- hashtable[[hash, 2]]+1
    lens <- hashtable[[hash, 1]]
    
    boxes[[box]] <- lens_logic(lens, boxes[[box]])
  }
  sum(unlist(lapply(boxes, \(x) focus_power(x, boxes))), na.rm = TRUE)

}


day15_2(input)

