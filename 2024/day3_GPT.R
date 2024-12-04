test <- readLines("~/advent-of-code/2024/data/day3_test", warn = FALSE)
input <- readLines("~/advent-of-code/2024/data/day3", warn = FALSE)



##first effort - wrong
#day3_1GPT <- function(input) {
#  # Use regular expressions to find valid mul(X,Y) patterns
#  matches <- gregexpr("mul\\(\\d+,\\d+\\)", input)
#  valid_muls <- regmatches(input, matches)[[1]]
#  
#  # Initialize the sum
#  total_sum <- 0
#  
#  # Process each valid mul
#  for (mul in valid_muls) {
#    # Extract the numbers within the parentheses
#    nums <- as.numeric(unlist(strsplit(gsub("mul\\(|\\)", "", mul), ",")))
#    # Multiply and add to the total
#    total_sum <- total_sum + nums[1] * nums[2]
#  }
#  
#  return(total_sum)
#}
#
#day3_1GPT(test)
##161
#
#day3_1GPT(input)
#28263962 <- output, wrong
#165225049 <- right answer :) 


#day3_1GPT <- function(input) {
#  # Use a more flexible regular expression to match `mul(X,Y)` with optional spaces
#  matches <- gregexpr("mul\\s*\\(\\s*\\d+\\s*,\\s*\\d+\\s*\\)", input, perl = TRUE)
#  valid_muls <- regmatches(input, matches)[[1]]
#  
#  # Initialize the sum
#  total_sum <- 0
#  
#  # Process each valid mul
#  for (mul in valid_muls) {
#    # Extract the numbers within the parentheses
#    nums <- as.numeric(unlist(strsplit(gsub("mul\\s*\\(|\\)", "", mul), "\\s*,\\s*")))
#    # Multiply and add to the total
#    total_sum <- total_sum + nums[1] * nums[2]
#  }
#  
#  return(total_sum)
#}
#
#day3_1GPT(test)
#161
#
#day3_1GPT(input)
#28263962 <- wrong answer again

#The below also doesn't work, but the issue is that it only reads a single line from the input
#day3_1GPT <- function(input) {
#  
#  # Use a precise regular expression to match valid mul(X,Y) patterns
#  matches <- gregexpr("mul\\(\\d+,\\d+\\)", input)
#  valid_muls <- regmatches(input, matches)[[1]]
#  
#  # Initialize the sum
#  total_sum <- 0
#  
#  # Process each valid mul
#  for (mul in valid_muls) {
#    # Extract the numbers within the parentheses
#    nums <- as.numeric(unlist(strsplit(gsub("mul\\(|\\)", "", mul), ",")))
#    
#    # Multiply and add to the total
#    total_sum <- total_sum + (nums[1] * nums[2])
#  }
#  
#  return(total_sum)
#  
#}


day3_1GPT <- function(input) {
  # Concatenate all lines of the input into a single string
  combined_input <- paste(input, collapse = "")
  
  # Use a precise regular expression to match valid mul(X,Y) patterns
  matches <- gregexpr("mul\\(\\d+,\\d+\\)", combined_input)
  valid_muls <- regmatches(combined_input, matches)[[1]]
  
  # Initialize the sum
  total_sum <- 0
  
  # Process each valid mul
  for (mul in valid_muls) {
    # Extract the numbers within the parentheses
    nums <- as.numeric(unlist(strsplit(gsub("mul\\(|\\)", "", mul), ",")))
    # Multiply and add to the total
    total_sum <- total_sum + nums[1] * nums[2]
  }
  
  return(total_sum)
}

day3_1GPT(test[1])
#161

day3_1GPT(input)
#165225049 got it right, well done ChatGPT :)



day3_2GPT <- function(input) {
  # Concatenate all lines of the input into a single string
  combined_input <- paste(input, collapse = "")
  
  # Use regex to find all relevant instructions (`mul`, `do`, `don't`)
  matches <- gregexpr("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)", combined_input)
  instructions <- regmatches(combined_input, matches)[[1]]
  
  # Initialize state and result
  enabled <- TRUE  # Multiplications are initially enabled
  total_sum <- 0   # Accumulator for the sum
  
  # Process each instruction
  for (instruction in instructions) {
    if (instruction == "do()") {
      enabled <- TRUE  # Enable future mul instructions
    } else if (instruction == "don't()") {
      enabled <- FALSE  # Disable future mul instructions
    } else if (startsWith(instruction, "mul") && enabled) {
      # Process valid `mul(X,Y)` if enabled
      nums <- as.numeric(unlist(strsplit(gsub("mul\\(|\\)", "", instruction), ",")))
      total_sum <- total_sum + nums[1] * nums[2]
    }
  }
  
  return(total_sum)
  
}

day3_2GPT(test[2])
#48

day3_2GPT(input)
#108830766 right first time, fresh out the box


