library(tidyverse)
source("~/advent-of-code/utils/utils.R")


#run this with the current day to add it into the dataset.

read_write <- function(day, sample = 100){
  data2 <- read_csv("~/advent-of-code/data/me-v-gpt.csv")
  data <- runner(day, sample)
  data3 <- bind_rows(data,data2)
  write_csv(data3, "~/advent-of-code/data/me-v-gpt.csv")
}









