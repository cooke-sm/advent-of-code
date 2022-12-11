library(tidyverse)

data <- readLines("~/advent-of-code/2022/inputs/day8.txt")

input_length <- length(data)
input_width <- nchar(data[1])


array_sum <- function(array){
  
  return(listrows = list(r = which(rowSums(array == 0)<input_length-1), 
                         c = which(colSums(array == 0)<input_length-1)))
  }

array_check <- function(array){
  
  output <- vector(mode = 'numeric', length = length(array))
  counter <- sum(array)
  write = 0
  
  for(i in seq_along(array)){
    
    if(array[i] == 0){
      output[i] <- write
    } else if((array[i] > 0) & (write==0)){
      output[i] <- write
      write <- 1
      counter <-  counter-array[i]
    } else if((array[i] > 0) & ((counter-array[i]) == 0)){
      write = 0
      output[i] <- write
      break
    } else if((array[i] > 0) & (write == 1)){
      counter <- (counter - array[i])
      output[i] <- write
    }
    
              
  }
  return(output)
}

z <- array(unlist(map(str_split(data, ""), parse_integer)), dim = c(input_width,input_length))
#got to add a one because you can't floor divide by zero
x <- z
z <- z+1


answer_array <- array(1, dim = c(input_width,input_length,10))

#The general gist of this is to floor divide by each level of trees
#and layer these on top of one another to build up a 'impression' left by the heights of trees
#then that creates a lookup table of whether or not that tree can be seen from outside
#Then just sum them all up and that's the answer

trees_left <- function(input, answer = answer_array){
  
  for(tree_level in c(10:1)){

    imp <- input%/%tree_level
    to_check <- array_sum(imp)
    row_ans <- array(0, c(input_width,input_length))

    for(i in to_check[["r"]]){
      input_array <- imp[i,]
      row_ans[i,] <- array_check(input_array)
    }
    
    col_ans <- array(0, c(input_width,input_length))
    
    for(i in to_check[["c"]]){
      input_array <- imp[,i]
      col_ans[,i] <- array_check(input_array)
    }
    trees_to_remove <- answer[,,tree_level]-(row_ans * col_ans)
    
    answer[,,tree_level] <- trees_to_remove
    input <-  input*trees_to_remove
  }
  
  return(answer)
}

answer_array <- trees_left(z)

final_answer <- array(1, dim = c(input_width,input_length))

for(i in seq(1:10)){
  matches <- which(z == i)
  array_index <- (input_length^2)*(i-1)
  
  for(match in matches){
    final_answer[match] <- answer_array[match+array_index]
  }
  
}

#part1
sum(final_answer)


#function that searches for all trees of a given height, then searches in each direction
#on the floor divided level until it hits a tree

scenic_find <- function(input){
  
  answer <- vector(mode = "integer")
  
  for(level in c(1:9)){
    to_check <- which(input==level,arr.ind = TRUE)
    tree_level <- input%/%level
    
    n_matches <- length(to_check)/2
    
    if(n_matches >0){
      up <- down <- left <- right <- 0
      
      for(item in seq(n_matches)){
        tree <- to_check[item,]
        row <- tree_level[tree[1],]
        col <- tree_level[,tree[2]]
        
        up <- scenic_check(col[tree[1]:1])
        down <- scenic_check(col[tree[1]:input_length])
        left <- scenic_check(row[tree[2]:1])
        right <- scenic_check(row[tree[2]:input_width])
        answer <-  append(answer, (up*down*left*right))
      }
      
    }
  }
  return(answer)
}


scenic_check <- function(vector_in){
  score <- 0
  for(i in vector_in[-1]){
    if(i == 0){
      score <- score+1
    } else if(i > 0){
      score <- score+1
      break
    }
  }
  return(score)
}

#part 2
max(scenic_find(x))