library(tidyverse)

inst <- readLines("~/advent-of-code/2022/inputs/day7test.txt")
input <- readLines("~/advent-of-code/2022/inputs/day7.txt")


#output needs to be the path, along with the sizes of the files. 
#That way you can group by directory
#and provide the answer

#To do this we'll need to map the the drives using vector, collapsed
#into a str_c function

inst_reader <- function(inst_line, current_level){
  if(str_detect(inst_line, "\\$ cd \\.\\.")){
    return(current_level[1:length(current_level)-1])
  } else if(str_detect(inst_line, "\\$ cd /")){
    return(c("/"))
  } else if(str_detect(inst_line, "\\$ cd [a-zA-Z]+")){
    return(append(current_level,str_extract(inst_line, "[a-zA-Z]$")))
  } else {
    return(current_level)
  }
}

main <- function(input){
  
  file_system <-  tibble(path = NULL, file = NULL, size = NULL)
  
  current_path <-  c("/")
  
  
  for(i in seq_along(input)){
    
    if(str_detect(input[i],"^\\$.")){
      current_path <- inst_reader(input[i], current_path)
    } else if(str_detect(input[i], "dir.")){
      file_system <- bind_rows(file_system, tibble(path = str_c(current_path,str_extract(input[i], "(?<! dir) .*"), collapse = "/")))
    } else {
      file_system <- bind_rows(file_system, tibble(path = str_c(current_path, collapse = "/"), 
                                    file = str_extract(input[i], "[^0-9]+$"),
                                    size = as.integer(str_extract(input[i], "^[0-9]+"))))
    }
  }
  return(file_system)
}
file <- main(inst)

file %>% drop_na()

sums <- tibble(path = NULL, sum_path = NULL)

for(i in seq(1:nrow(file))){
  sizesum = 0
  for(p in seq(1:nrow(file)))
    if(str_detect(file$path[p],file$path[i])){
      sizesum = sizesum + file$size
    }
  sums <- bind_rows(sums, tibble(path = file$path[i], sum_path = sizesum))
}

sums %>% 
  filter(sum_path < 10000) %>% 
  summarise(s = sum(sum_path, na.rm = TRUE))


