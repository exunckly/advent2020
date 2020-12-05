# Day 1: Advent of Code 2020

# Read in numbers, find a pair (part 1) or triplet (part 2) that sums to 2020, then find the product of those two/three numbers

library(tidyverse)

#input_filename <- "data/d1_p1_test.txt"
input_filename <- "data/d1_p1_input.txt"

# Make all possible combinations of the numbers using expand_grid
# Cut down on unnecessary calculations using filter
initial_data <- read_csv(input_filename, col_names = FALSE)

# Part 1 - all possible combinations of 2 values
my_data <- expand_grid(X1 = initial_data$X1, X2 = initial_data$X1) %>%
  mutate(my_sum = X1 + X2) %>%
  filter(my_sum == 2020) %>%
  mutate(my_product = X1 * X2)

print(paste0("Part 1 answer: ", my_data$my_product[1]))


# Part 2 - all possible combinations of 3 values
my_data <- expand_grid(X1 = initial_data$X1, X2 = initial_data$X1, X3 = initial_data$X1) %>%
  mutate(my_sum = X1 + X2 + X3) %>%
  filter(my_sum == 2020) %>%
  mutate(my_product = X1 * X2 * X3)

print(paste0("Part 2 answer: ", my_data$my_product[1]))
