test <- readLines("~/advent-of-code/2024/data/day5_test")
input <- readLines("~/advent-of-code/2024/data/day5")


dict_builder <- function(rules){
  #this takes the 'rules' input and returns a dict-like data structure
  output <- list()
  
  for(rule in rules){
    split <- strsplit(rule, "\\|")[[1]]
    
    if(split[1] %in% names(output)){
      
      #making sure you get the square parentheses right, my most hated thing about R

      output[[split[1]]] <- c(output[[split[1]]], as.numeric(split[2]))
      
    } else {
      output[split[1]] <- as.numeric(split[2])
    }
  }
 output 
}

day5_1 <- function(input){
  
  bp <- which(match(input, "") == 1)
  rules <- input[1:(bp-1)]
  updates <- lapply(strsplit(input[(bp+1):length(input)], "," ), as.numeric)
  
  dict <- dict_builder(rules)
  
  checker <- function(updates){
    for(i in seq(length(updates),1)){ #go backwards
      must_come_after <- dict[[paste(updates[i])]]
    
      if(any(must_come_after %in% updates)){
        return(FALSE)
        break
      }
      else {
        updates <- updates[-i]
      }
    }
    TRUE
  }
  correct <- updates[unlist(lapply(updates, checker))]
  
  sum(as.numeric(unlist(lapply(correct, \(x) x[(length(x)%/%2)+1]))))
}

day5_1(test)
#143

day5_1(input)
#6498

swapper <- function(j,update){
  
  old <- update[j-1]
  update[(j-1):j] <- c(update[j],old)
  
  update
}

day5_2 <- function(input){
  
  bp <- which(match(input, "") == 1)
  rules <- input[1:(bp-1)]
  updates <- lapply(strsplit(input[(bp+1):length(input)], "," ), as.numeric)
  
  dict <- dict_builder(rules)
  
  checker <- function(updates){
    
    
    for(i in seq(length(updates),1)){ #go backwards
      
      must_come_after <- dict[[paste(updates[i])]]
      
      if(any(must_come_after %in% updates)){
        return(FALSE)
        break
      }
      else {
        updates <- updates[-i]
      }
    }
    TRUE
  }
  
  incorrect <- updates[!unlist(lapply(updates, checker))]
  
  rec_check <- function(j, updates = updates, lookup){
    
    jloc <- match(j,updates, lookup)
    
    if(!any(lookup %in% updates[1:jloc])){
      return(updates)
    } else {
      updates <- swapper(jloc,updates)
      
      return(rec_check(j,updates, lookup))
    }
    
  }
  
  fixer <- function(updates){
    
    for(j in rev(updates)){
      
      lookup <- dict[[paste(j)]]
      
      updates <- rec_check(j, updates, lookup)
      
    }
    updates
  }
  
  reordered <- lapply(incorrect, fixer)
  
  sum(as.numeric(unlist(lapply(reordered, \(x) x[(length(x)%/%2)+1]))))
  
  
}

day5_2(test)
#123

day5_2(input)
#5017
