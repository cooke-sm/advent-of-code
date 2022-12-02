library(tidyverse)

# Rock A / X - 1
# Paper B / Y - 2
# Scissors C / Z - 3
# 0 for a lose, 3 for a draw, 6 for win

#rename cols: o for opponent, p for player
df <- read_fwf("~/advent-of-code/2022/inputs/day2.txt") %>% 
  rename("o"=X1, "p"= X2)

mapping = c("A" = 1,"B" = 2,"C" = 3, "X" = 1, "Y" = 2, "Z" = 3)

df %>% mutate(o = mapping[o],
                p = mapping[p],
                outcome = ((p+1-o)%%3)*3,
                answer = p+outcome) %>% 
    summarise(sum(answer))

#part 2
# Rock A / 1
# Paper B / 2
# Scissors C / 3
# X = lose, Y = draw, Z = win
# 0 for a lose, 3 for a draw, 6 for win


#gotta change the mapping order so that it works. I'm sure there's a better way.
mapping2 = c("A" = 1,"B" = 2,"C" = 3, "X" = 1, "Y" = 3, "Z" = 2)

df %>% mutate(winscore = (mapping[p]-1)*3,
              o = mapping2[o],
              p = mapping2[p],
                outcome = 3-((p-o)%%3),
                answer = winscore+outcome) %>% 
  summarise(sum(answer))



