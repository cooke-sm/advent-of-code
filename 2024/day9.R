test <- readLines("~/advent-of-code/2024/data/day9_test")
input <- readLines("~/advent-of-code/2024/data/day9")



space <- function(input){
  rep(".",as.numeric(input))
}

file <- function(input, position){
  rep(position,as.numeric(input))
}

swapper <- function(unzip){
  spaces <- which(unzip == ".", arr.ind = TRUE)
  files <- which(unzip != ".", arr.ind = TRUE)
  
  unzip[min(spaces)] <- unzip[max(files)]
  unzip[max(files)] <- "."
  
  unzip
  
}

finder <- function(block, drive){
  if(grepl("\\.", block)){#this is redundant now
    
    seeking <- nchar(block)
    
    for(i in seq(length(drive), 1)){
      
      replacement <- nchar(drive[i])
      
      if(replacement <= seeking){
        
        return(c(drive[i], rep(".", seeking-replacement)))
        
      }
      
    }
    
    
  } 
  
  return(block)
}

day9_1 <- function(input){
  raw <- strsplit(input, "")[[1]]
  
  output <- list()
  pos <- 0
  file <- TRUE
  
  for(i in seq_along(raw)){
    
    if(file == TRUE){
      output[[i]] <- file(raw[i],pos)
      pos <- pos+1
      file <- FALSE
      
    } else {
      output[[i]] <- space(raw[i])
      file <- TRUE
    }
    
  }
  unzip <- unlist(output)
  
  while(min(which(unzip == ".", arr.ind = TRUE)) < max(which(unzip != ".", arr.ind = TRUE))){
    unzip <- swapper(unzip)
    
  }
  unzip
  
  pos <- 0
  ans <- c()
  
  for(j in seq_along(unzip)){
    
    if(unzip[j] != "."){
      ans[j] <- pos*as.numeric(unzip[j])
      pos = pos+1
    }
  }
  
  sum(ans)
}

day9_1(test)
#1928

format(day9_1(input), scientific = FALSE)
#6242766523059



day9_2 <- function(input){
  raw <- strsplit(input, "")[[1]]
  
  output <- list()
  pos <- 0
  file <- TRUE
  
  for(i in seq_along(raw)){
    
    if(file == TRUE){
      output[[i]] <- file(raw[i],pos)
      pos <- pos+1
      file <- FALSE
      
    } else {
      output[[i]] <- space(raw[i])
      file <- TRUE
    }
    
  }
  
  unzipped <- unlist(lapply(output, paste, collapse = ""))
  output <- unzipped
  
  while(min(which(grepl("\\.", output), arr.ind = TRUE)) < max(which(grepl("[0-9]", output), arr.ind = TRUE))){
    
  }

  
 output
}

x <- day9_2(test)


finder(".", c("a", "b", "c", "ddd"))
