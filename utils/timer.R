timer <- function(f){
  start <- Sys.time()
  f
  end <- Sys.time()
  
  end-start
}