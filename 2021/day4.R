test <- readLines("~/advent-of-code/2021/data/day4_test")
input <- readLines("~/advent-of-code/2021/data/day4")

grid_reader <- function(grid){
  cleaned <- lapply(grid, \(x) stringr::str_extract_all(x, "\\d+", simplify = TRUE))
  numbers <- lapply(cleaned, as.numeric)
  do.call(rbind, numbers)
}

grid_check <- function(grid){
  #returns TRUE if there's a line
  to_check <- is.na(grid)
  
  if(any(rowSums(to_check) == 5) | any(colSums(to_check) == 5)){
    TRUE
  } else {
    FALSE
    }
}

day4_1 <- function(input){
  numbers <- as.numeric(strsplit(input[1], ",")[[1]])
  
  #reads the grids into a list
  grids <- input[2:length(input)]
  groups <- cumsum(grids == "")
  grids <- split(grids[grids != ""], groups[grids != ""])
  grids <- lapply(grids, grid_reader)
  
  #this is the main loop
  for(i in seq(1, length(numbers))){
    
    called <- numbers[i]
    
    #use NA to mark the called numbers off
    grids <- lapply(grids, function(x) {

      x[x==called] <- NA
      return(x)
    }
    )
    checked <- lapply(grids, grid_check)
    
      if(any(checked)) {
        ans <- sum(grids[checked==TRUE][[1]], na.rm = TRUE)*called
        break
      }
    
  }
  ans
}

day4_1(test)
#4512

day4_1(input)
#71708


day4_2 <- function(input){
  numbers <- as.numeric(strsplit(input[1], ",")[[1]])
  
  #reads the grids into a list
  grids <- input[2:length(input)]
  groups <- cumsum(grids == "")
  grids <- split(grids[grids != ""], groups[grids != ""])
  grids <- lapply(grids, grid_reader)
  
  #this is the main loop
  for(i in seq(1, length(numbers))){
    
    called <- numbers[i]
    
    grids <- lapply(grids, function(x) {
      
      x[x==called] <- NA
      
      return(x)
      
    }
    )
    checked <- lapply(grids, grid_check)
    
    
    #This section changed for part 2: if ANY have changed, take them out of the list of grids
    #once ALL have got a line, then return that. TBH you don't need the all, you could just return it
    #once n=1 but I'm sure it's fine :)
    
    if(all(checked)){ 
      ans <- sum(grids[checked==TRUE][[1]], na.rm = TRUE)*called
      break
    } else if(any(checked)) {
      grids <- grids[checked == FALSE]
    }
    
  }
  ans
}

day4_2(test)
#1924

day4_2(input)
#34726





