# Day 1: Advent of Code 2020

# part 1
# Read in numbers, find a pair that sums to 2000, then find the product of that pair

library(tidyverse)

input_filename <- "data/d1_p1_test.txt"
input_filename <- "data/d1_p1_test.txt"

# Make all possible combinations of the numbers using expand_grid
# Cut down on unecessary calculations using filter
my_data <- read_csv(input_filename) %>%
  expand_grid() %>%
  mutate(my_sum = x1 + x2) %>%
  filter(my_sum == 2000) %>%
  mutate(my_product = x1*x2)

print(paste0("Part 1 answer: ", my_data$my_product[1]))

