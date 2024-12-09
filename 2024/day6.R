test <- readLines("~/advent-of-code/2024/data/day6_test")
input <- readLines("~/advent-of-code/2024/data/day6")


mat_rotate <- function(mat){
  #rotates the matrix 90 degrees
  t(mat)[nrow(mat):1,]
}

day6_1 <- function(input){
  
  mat <- do.call(rbind, strsplit(input, ""))
  exit <- FALSE
  
  while(!exit){
    
    start_pos <- which(mat == "^", arr.ind = TRUE)
    path <- mat[,start_pos[2]]
    
    if("#" %in% path[1:start_pos[1]]){
      
      end <- max(which(path[1:start_pos[1]] == "#"))
      path[(end+1):start_pos[1]] <- "x"
      path[(end+1)] <- "^"
      
    } else {
      
      exit <-  TRUE
      path[1:start_pos[1]] <- "x"
      
    }
    
    mat[,start_pos[2]] <- path
    mat <- mat_rotate(mat)
    
  }
  
  sum(stringr::str_count(mat, "x"))
  
}

day6_1(test)
#41

day6_1(input)
#4789

walk_path <- function(mat){
  
  exit <- FALSE
  i <- 0
  
  while(!exit){
    
    
    #print(stringr::str_c("iteration: ", i))
    start_pos <- which(mat == "^", arr.ind = TRUE)
    path <- mat[,start_pos[2]]
    
    
    if("#" %in% path[1:start_pos[1]]){
      
      end <- max(which(path[1:start_pos[1]] == "#"))
      
      if(all(path[(end+1):(start_pos[1]-1)] == "x")){
        
        exit <- TRUE
        
        return("infinite loop")
      } else {
        
        path[(end+1):start_pos[1]] <- "x"
        path[(end+1)] <- "^"
      }
      
    } else {
      
      exit <-  TRUE
      path[1:start_pos[1]] <- "x"
      
      
    }
    
    mat[,start_pos[2]] <- path
    
    if((i%%4 != 0) & exit){
      mat <-  Reduce(function(x, ...) mat_rotate(x), c(1:(i%%4)), init = mat)
    } else{
      mat <- mat_rotate(mat)
    }
    
    i <- i+1
    
  }
  
  mat
}



day6_2 <- function(input){
  
  mat <- do.call(rbind, strsplit(input, ""))
  exit <- FALSE
  
  start_pos <- which(mat == "^", arr.ind = TRUE)
  locs_visit <- which(walk_path(mat) == "x", arr.ind = TRUE)
  
  #locs_visit <- locs_visit[-which(locs_visit[,1] == start_pos[1] & locs_visit[,2] == start_pos[2], arr.ind = TRUE),]
  
  output <- list()
  blocked <- mat
  
  for(loc in seq(1, nrow(locs_visit))){
    
    blocked <- mat
    
    
    row <- locs_visit[loc,1]
    col <- locs_visit[loc,2]
    
    blocked[row,col] <- "#"
    
    output[loc] <- walk_path(blocked)
    
  }
  
  sum(stringr::str_count(unlist(output), "infinite loop"))
}



day6_2(test)
#6

day6_2(input)



      
