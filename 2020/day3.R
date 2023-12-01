test_input <- 
"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"


day3_1 <-  function(input){
  
  list <- strsplit(input,"\n")
  
  x = nchar(list[[1]][[1]])
  y = length(list[[1]])

  parsed <- lapply(list, \(x) stringr::str_replace_all(x, c("#"="1", "\\." = "0")))
  numbers <- lapply(lapply(parsed, strsplit, "")[[1]], readr::parse_number)
  tree_mat <- matrix(unlist(numbers),nrow = x, ncol = y)
  
  answer = 0
  row_count = 1
  
  
  for(col_count in seq(2,y)){
  
    if(((row_count + 3)%%x) > 0){row_count = (row_count + 3)%%x}
    else{row_count = x}
    
    answer = answer+tree_mat[row_count,col_count]
    
  }
  
  answer
  
}

day3_1(test_input)

input <- readr::read_file("~/advent-of-code/2020/inputs/day3.txt")

day3_1(input)


day3_2 <-  function(input, input_x, input_y){
  
  list <- strsplit(input,"\n")
  
  x = nchar(list[[1]][[1]])
  y = length(list[[1]])
  
  parsed <- lapply(list, \(x) stringr::str_replace_all(x, c("#"="1", "\\." = "0")))
  numbers <- lapply(lapply(parsed, strsplit, "")[[1]], readr::parse_number)
  tree_mat <- matrix(unlist(numbers),nrow = x, ncol = y)
  
  answer = 0
  row_count = 1
  yswitch = FALSE
  
  
  for(col_count in seq(2,y)){
    
    if((col_count+1)%%input_y == 0){
    
    if(((row_count + input_x)%%x) > 0){row_count = (row_count + input_x)%%x}
    else{row_count = x}
    
    answer = answer+tree_mat[row_count,col_count]
  
    }
   else {}
  }
  
  answer
  
}

day3_2(input,1,1)*
day3_2(input,3,1)*
day3_2(input,5,1)*
day3_2(input,7,1)*
day3_2(input,1,2)


1005015792

6298701948