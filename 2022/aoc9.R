library(tidyverse)

#> The plan is to calculate all the positions t could move in a grid around it
#> if the grids overlap by more than 3, don't do anything
#> But if they overlap less than 4, calculate the distance from h to all those potential positions
#> and t's new position becomes the shortest distance

input <- read.table("2022/inputs/day9test.txt")
instr <- rep(input$V1, input$V2)

grid <- expand_grid(x = (-1:1), y = (-1:1))

distance <- function(h, t){
  x <- c(h)-c(t)
  y <- sqrt(sum(x^2))
  return(y)
}

new_position <- function(t,h, g = grid){
  
  t_grid <- g %>% mutate(x=x+t[1],y=y+t[2])
  h_grid <- g %>% mutate(x=x+h[1],y=y+h[2])
  
  if(nrow(intersect(t_grid,h_grid))<4){
    new_pos <- t_grid %>% 
      rowwise() %>% 
      mutate(dist = distance(c(x,y),h)) %>% 
      ungroup() %>% 
      filter(dist == min(dist)) %>% 
      unlist()
    
    t <- c(new_pos[1],new_pos[2])
  }
  
  return(t)
}
  
dir_switch <- function(ins, h_coords){
  new_coords <- switch(ins,
                       "U" = h_coords+c(0,1),
                       "D" = h_coords-c(0,1), 
                       "L" = h_coords-c(1,0),
                       "R" = h_coords+c(1,0)) 
  return(new_coords)
}

t <- h <- c(x=0,y=0)
answer <- tibble(x=0,y=0)


for(i in instr){
  h <- dir_switch(i,h)
  t <- new_position(t,h)
  answer <- bind_rows(answer,t)
}

unique(answer) %>% nrow()

answer2 <- tibble(x=0,y=0,k=0)
t <- h <- c(x=0,y=0)
q <- tibble(x=rep(0,10),y=rep(0,10))


for(i in instr){
  
  h2 <- h <- dir_switch(i,h)
  q[1,] <- as.list(h)
  

  for(new in c(2:10)){
    h <- unlist(q[new-1,])
    t <- unlist(q[new,])
    t <- new_position(t,h)
    q[new,] <- as.list(t)
    t["k"] <- new
    answer2 <- bind_rows(answer2,t)
  }
  
  for(p in c(10:2)){
    q[p,] <- as.list(q[p-1,])
  }
  
  h <- h2
}


answer2 %>% 
  filter(k == 2) %>% view()
  unique() %>% 
  nrow()


