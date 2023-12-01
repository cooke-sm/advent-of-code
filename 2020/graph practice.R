library(igraph)
library(tidyverse)


data <- tibble(a = c("A","A","B","C"),
               b = c("B", "C", "A", "A"))


g <- graph.edgelist(as.matrix(data))

plot(g)

igraph::