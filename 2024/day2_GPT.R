test <- readLines("~/advent-of-code/2024/data/day2_test", warn = FALSE)
input <- readLines("~/advent-of-code/2024/data/day2", warn = FALSE)


day2_1GPT <- function(input) {
  # Ensure the input is a character string
  if (!is.character(input)) {
    stop("Input must be a character string.")
  }
  
  # Split the input into lines
  lines <- unlist(strsplit(input, "\\n"))
  
  # Parse each line into a numeric vector
  reports <- lapply(lines, function(line) as.numeric(unlist(strsplit(line, " "))))
  
  # Function to check if a report is safe
  is_safe <- function(report) {
    # Calculate differences between adjacent levels
    diffs <- diff(report)
    
    # Check if all differences are either >= 1 and <= 3
    valid_differences <- all(abs(diffs) >= 1 & abs(diffs) <= 3)
    
    # Check if all differences are positive (increasing) or negative (decreasing)
    monotonic <- all(diffs > 0) || all(diffs < 0)
    
    # The report is safe if both conditions are met
    valid_differences && monotonic
  }
  
  # Count the number of safe reports
  safe_count <- sum(sapply(reports, is_safe))
  
  return(safe_count)
}

day2_1GPT(test)
#2

day2_1GPT(input)
#224

day2_2GPT <- function(input) {
  # Ensure the input is a character string
  if (!is.character(input)) {
    stop("Input must be a character string.")
  }
  
  # Split the input into lines
  lines <- unlist(strsplit(input, "\\n"))
  
  # Parse each line into a numeric vector
  reports <- lapply(lines, function(line) as.numeric(unlist(strsplit(line, " "))))
  
  # Function to check if a report is safe
  is_safe <- function(report) {
    # Calculate differences between adjacent levels
    diffs <- diff(report)
    
    # Check if all differences are either >= 1 and <= 3
    valid_differences <- all(abs(diffs) >= 1 & abs(diffs) <= 3)
    
    # Check if all differences are positive (increasing) or negative (decreasing)
    monotonic <- all(diffs > 0) || all(diffs < 0)
    
    # The report is safe if both conditions are met
    valid_differences && monotonic
  }
  
  # Function to check if a report can be made safe by removing one level
  can_be_made_safe <- function(report) {
    n <- length(report)
    # Try removing each level and check if the remaining report is safe
    for (i in seq_len(n)) {
      modified_report <- report[-i]
      if (is_safe(modified_report)) {
        return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # Count the number of safe reports
  safe_count <- sum(sapply(reports, function(report) {
    is_safe(report) || can_be_made_safe(report)
  }))
  
  return(safe_count)
}

day2_2GPT(test)
#4

day2_2GPT(input)
#293
