library(tidyverse)


test <- readLines("~/advent-of-code/2023/data/day4_test.txt")
input <- readLines("~/advent-of-code/2023/data/day4_input.txt")

day4_1 <- function(input) {
  tibble(raw = input) |>
    mutate(
      card = parse_number(str_extract(raw, "Card \\d+")),
      win_no = str_extract(raw, "(?<=:\\s).*?(?=\\s\\|)"),
      scratch_no = str_extract(raw, "(?<=\\|\\s).*?$"),
      win_no = lapply((str_split(win_no, "\\s+")), parse_number, na = c("", character())),
      scratch_no = lapply((str_split(scratch_no, "\\s+")), parse_number, na = c("", character())),
    ) |>
    select(card, win_no, scratch_no) |>
    rowwise() |>
    mutate(
      inter = list(intersect(win_no, scratch_no)),
      inter = list(inter[!is.na(inter)]), # NAs creeping in!
      length = length(inter),
      points = case_when(
        length > 0 ~ 2^(length - 1),
        length == 0 ~ 0
      )
    ) |>
    ungroup() |>
    pull(points) |>
    sum()
}

# answer
day4_1(input)
