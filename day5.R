# Day 5: Advent of Code 2020

library(tidyverse)

#input_filename <- "data/d5_test.txt"
input_filename <- "data/d5_input.txt"

# This is binary, with B or R as 1 and F or L as 0
# Use strtoi to get the binary values from strings

# Process data to get row, column and seat IDs
my_data <- read_csv(input_filename, col_names = FALSE) %>%
  mutate(binary = str_replace_all(X1, "F|L", "0")) %>%
  mutate(binary = str_replace_all(binary, "B|R", "1")) %>%
  mutate(row = strtoi(str_sub(binary, 1, 7), base = 2)) %>%
  mutate(column = strtoi(str_sub(binary, 8, 10), base = 2)) %>%
  mutate(seat_id = row * 8 + column)

# Part 1 solution - highest seat ID
print(paste0("Highest seat ID: ", max(my_data$seat_id)))

# Part 2 solution - use setdiff to locate missing seat ID that is not at the beginning or end of the sequence
seat_range <- min(my_data$seat_id):max(my_data$seat_id)
print(paste0("My seat ID: ", setdiff(seat_range, my_data$seat_id)))