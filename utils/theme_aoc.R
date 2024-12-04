library(tidyverse)
library(showtext)

aoc_colours <- c("#CCCCCC", "#0f0f23", "#00CC00", 
                 "#009900","#FFFF66","#FFFFFF",
                 "#666666")

font_add_google(name = "Source Code Pro", family = "source_code_pro")
showtext_auto()


theme_aoc <- function(){
  theme(text = element_text(family = "source_code_pro", colour = "#CCCCCC", size = 35),
        rect = element_rect(fill = "#0f0f23"),
        panel.background = element_blank(),
        panel.grid = element_line(colour = aoc_colours[7]),
        axis.text = element_text(colour = aoc_colours[1]),
        plot.title = element_text(colour = aoc_colours[6]),
        plot.subtitle = element_text(size = rel(.8)),
        plot.caption = element_text(colour = aoc_colours[4]),
        axis.line.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = aoc_colours[3]),
        axis.text.y = element_text(colour = aoc_colours[3]),
        panel.border = element_rect(colour = aoc_colours[7], fill = NA)
        
  )
}
