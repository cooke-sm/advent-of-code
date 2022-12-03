
data <- readLines(con = "~/advent-of-code/2022/inputs/day3.txt")

#mapping to get letter scores
mapping <- append(letters, LETTERS)

#function which splits the string into two sections then takes their intersection
backpack_match <- function(backpack){
  comp1 <- strsplit(substr(backpack[1],1,(nchar(backpack[1])/2)), "")[[1]]
  comp2 <- strsplit(substr(backpack[1],(nchar(backpack[1])/2)+1,nchar(backpack[1])),"")[[1]]
  return(match(intersect(comp1,comp2),mapping))
}
#answer 1

sum(sapply(data,backpack_match))


#answer 2

triples <- split(data, ceiling(seq_along(data)/3))
  
#same principle as above (taking intersection) but with an extra iteration
priority_finder <- function(triple){
  x = sapply(triple,strsplit,"")
  y = intersect(x[[1]],x[[2]])
  z = intersect(x[[3]],y)
  return(match(z,mapping))
}

sum(sapply(triples, priority_finder))

