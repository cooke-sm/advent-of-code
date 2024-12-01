test <- readLines("~/advent-of-code/2024/data/day1_test", warn = FALSE)
input <- readLines("~/advent-of-code/2024/data/day1", warn = FALSE)
source("~/advent-of-code/utils/utils.R")

### attempt 1 ###
#This doesn't work because the input is in character format.
#
#day1_1GPT <- function(input) {
#  # Split the input into two separate lists
#  left_list <- input[[1]]
#  right_list <- input[[2]]
#  
#  # Sort both lists
#  sorted_left <- sort(left_list)
#  sorted_right <- sort(right_list)
#  
#  # Calculate the absolute differences between corresponding elements
#  differences <- abs(sorted_left - sorted_right)
#  
#  # Sum up the differences to get the total distance
#  total_distance <- sum(differences)
#  
#  # Return the total distance
#  return(total_distance)
#}
#
#day1_1GPT(test)

### attempt 2 ###

#day1_1GPT <- function(input) {
#  # Split the input string into individual lines
#  lines <- strsplit(input, "\n")[[1]]
#  
#  # Parse the two lists from the input
#  left_list <- numeric()
#  right_list <- numeric()
#  
#  for (line in lines) {
#    # Split each line by whitespace and convert to numeric
#    numbers <- as.numeric(unlist(strsplit(line, "\\s+")))
#    left_list <- c(left_list, numbers[1])
#    right_list <- c(right_list, numbers[2])
#  }
#  
#  # Sort both lists
#  sorted_left <- sort(left_list)
#  sorted_right <- sort(right_list)
#  
#  # Calculate the absolute differences between corresponding elements
#  differences <- abs(sorted_left - sorted_right)
#  
#  # Sum up the differences to get the total distance
#  total_distance <- sum(differences)
#  
#  # Return the total distance
#  return(total_distance)
#}

#day1_1GPT(test)
#outputs 1


### attempt 3 ###

# I had to help it here, on line 69, it was trying to subset the list simlar to line 32 above. 
# Removing this makes the code work


day1_1GPT <- function(input) {
  # Split the input string into individual lines
  lines <- strsplit(input, "\n")
  
  # Initialize empty numeric vectors for the left and right lists
  left_list <- numeric()
  right_list <- numeric()
  
  # Parse the two lists from the input
  for (line in lines) {
    # Trim whitespace and split each line by one or more spaces
    numbers <- as.numeric(unlist(strsplit(trimws(line), "\\s+")))
    # Append the first and second numbers to their respective lists
    left_list <- c(left_list, numbers[1])
    right_list <- c(right_list, numbers[2])
  }
  
  # Sort both lists
  sorted_left <- sort(left_list)
  sorted_right <- sort(right_list)
  
  # Calculate the absolute differences between corresponding elements
  differences <- abs(sorted_left - sorted_right)
  
  # Sum up the differences to get the total distance
  total_distance <- sum(differences)
  
  # Return the total distance
  return(total_distance)
  
}

day1_1GPT(test)
#11
day1_1GPT(input)
#3569916 - correct answer


####### part 2 #######

### attempt 1 ###

#I'm going help it out with the parsing again as it couldn't quite get it's head around the mistake last time

day1_2GPT <- function(input){
  # Split the input string into individual lines
  lines <- strsplit(input, "\n")#[[1]] <-  human intervention to remove this
  
  # Initialize empty numeric vectors for the left and right lists
  left_list <- numeric()
  right_list <- numeric()
  
  # Parse the two lists from the input
  for (line in lines) {
    # Trim whitespace and split each line by one or more spaces
    numbers <- as.numeric(unlist(strsplit(trimws(line), "\\s+")))
    # Append the first and second numbers to their respective lists
    left_list <- c(left_list, numbers[1])
    right_list <- c(right_list, numbers[2])
  }
  
  # Calculate the similarity score
  similarity_score <- 0
  
  # Count occurrences of each number in the right list
  right_list_counts <- table(right_list)
  
  # Iterate through the left list
  for (num in left_list) {
    # If the number appears in the right list, add to the similarity score
    if (num %in% names(right_list_counts)) {
      count_in_right <- as.numeric(right_list_counts[as.character(num)])
      similarity_score <- similarity_score + num * count_in_right
    }
  }
  
  # Return the similarity score
  return(similarity_score)
}


day1_2GPT(test)
#31
day1_2GPT(input)
#26407426
