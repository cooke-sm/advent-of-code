library(tidyverse)

input <- read.table("2022/inputs/day10.txt", fill=TRUE)
input <- bind_rows(data.frame(counter = 0, V1 = as.character(NA), V2 = 1, clock = 0, x = 1), input)


#part1

input %>% 
  mutate(clock = case_when(V1 == "addx" ~ 2,
                           V1 == "noop" ~ 1,
                           TRUE ~ clock),
         V2 = case_when(is.na(V2)~ 0,
                        TRUE ~ V2),
         x = cumsum(V2)) %>% 
  mutate(counter = cumsum(clock)) -> df

  
answer <- data.frame('counter' = c(0:240))

answer2 <- left_join(answer, df) %>% 
  mutate(x = lag(x)) %>% 
  fill(x, .direction = "down")

answer2 %>% 
  filter(counter %in% c(20,60,100,140,180,220)) %>%
  select(x,counter) %>%
  mutate(ans = x*counter) %>%
  summarise(sum(ans))

#part2

answer2 <- left_join(answer, df) %>% 
  fill(x, .direction = "down")

sprite <- c(-1,0,+1)

answer2 %>% 
  rowwise() %>% 
  mutate(output = case_when((counter%%40) %in% c(sprite+x) ~ "#",
                            TRUE ~ "."),
         layer = counter%/%40) %>% 
  group_by(layer) %>% 
  summarise((str_c(output,collapse="")))
