library(tidyverse)
library(showtext)

aoc_colours <- c("#CCCCCC", "#0f0f23", "#00CC00", 
                 "#009900","#FFFF66","#FFFFFF",
                 "#666666")

font_add_google(name = "Source Code Pro", family = "source_code_pro")
showtext_auto()


theme_aoc <- function(){
  theme(text = element_text(family = "source_code_pro", colour = "#CCCCCC"),
               rect = element_rect(fill = "#0f0f23"),
               panel.background = element_blank(),
               panel.grid = element_line(colour = aoc_colours[7]),
               axis.text = element_text(colour = aoc_colours[1]),
               plot.title = element_text(colour = aoc_colours[6], size = 30),
               plot.caption = element_text(colour = aoc_colours[4])
  )
}
