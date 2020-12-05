# Day 2: Advent of Code 2020

library(tidyverse)
#input_filename <- "data/d2_p1_test.txt"
input_filename <- "data/d2_p1_input.txt"

initial_data <- read_csv(input_filename, col_names = FALSE)

# Part 1
# Format of data is range for no. of times a letter appears in a string, the letter, then the string
# Final answer is number of valid passwords

my_data <- initial_data %>%
  separate(X1, into = c("X1", "X2", "letter", "password")) %>%
  mutate_at("X1", as.numeric) %>%
  mutate_at("X2", as.numeric) %>%
  # Count matching letters, test whether within the range
  mutate(n = str_count(password, letter)) %>%
  mutate(part_1_valid = (n >= X1 & n <= X2))

part_1 <- my_data %>%
  count(part_1_valid) %>%
  filter(part_1_valid == TRUE)

print(paste0("Part 1 - number of valid passwords: ", part_1$n[1]))

# Part 2
# Same data as before but format is 2 positions in a string where a letter might be and a letter
# Password is valid if letter appears in exactly one position

my_data <- my_data %>%
  mutate(pos1 = str_sub(password, X1, X1)) %>%
  mutate(pos2 = str_sub(password, X2, X2)) %>%
  mutate(part_2_valid = xor(letter == pos1, letter == pos2))
  
part_2 <- my_data %>%
  count(part_2_valid) %>%
  filter(part_2_valid == TRUE)

print(paste0("Part 2 - number of valid passwords: ", part_2$n[1]))
  
