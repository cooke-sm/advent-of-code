library(tidyverse)
source("~/advent-of-code/utils/utils.R")
source("~/advent-of-code/utils/theme_aoc.R")


#run this with the current day to add it into the dataset.

read_write <- function(day, sample = 100){
  data2 <- read_csv("~/advent-of-code/data/me-v-gpt.csv")
  data <- runner(day, sample)
  data3 <- bind_rows(data,data2)
  write_csv(data3, "~/advent-of-code/data/me-v-gpt.csv")
}


#wips below


data |> pivot_longer(cols = c(me,gpt)) |>
  ggplot(aes(x = value, y = fct_rev(day), colour = name))+
  geom_jitter(height = .3, size = .5)+
  scale_colour_manual(values = aoc_colours[4:5], guide = guide_legend(title = NULL))+
  facet_wrap(~part)+
  theme_aoc()+
  xlab("Speed in ms")+
  labs(title = "Advent of Code - me vs ChatGPT",
       subtitle = "Every dot is a single run of the program which provides the right answer",
       caption = "Written in R | code at cooke-sm/advent-of-code")

ggsave("meVgpt-day3.jpg", device = 'jpg', path = "~/advent-of-code/plots", 
       width = 8, height = 5, dpi = 240)



  





