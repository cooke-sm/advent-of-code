timer <- function(f){
  start <- Sys.time()
  f
  end <- Sys.time()
  
  as.numeric(end-start)
}


runner <- function(day, sample = 100){
  
  path <- paste("~/advent-of-code/2024/")
  files <- list.files(path)
  
  for(file in files[str_detect(files, day)]){
    source(paste0(path,file))
  }
  
  #internal function definitions feel a bit dirty
  #but it makes the calls below a bit cleaner
  f_call <- function(day, part, who = ""){
    do.call(paste0(day, "_", part, who), list(input))
  }
  
  part1 <- tibble(me = replicate(n = sample, timer(f_call(day,"1"))),
                  gpt = replicate(n = sample, timer(f_call(day,"1","GPT"))),
                  part = "Part 1")
  
  part2 <- tibble(me = replicate(n = sample, timer(f_call(day,"2"))),
                  gpt = replicate(n = sample, timer(f_call(day,"2","GPT"))),
                  part = "Part 2")
  
  bind_rows(part1,part2) %>% 
    mutate(day = day)
  
}