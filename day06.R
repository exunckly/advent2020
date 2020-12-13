# Day 6: Advent of Code 2020
library(tidyverse)

input_filename <- "data/d06_test.txt"
#input_filename <- "data/d06_input.txt"

# Part 1
# We want to know how many lower case letters are covered for each grouo of passengers in the entire dataset
# We do not currently care about individual passengers

my_data<- tibble(group_data = read_file(input_filename)) %>%
  separate_rows (group_data, sep = "\n\n") %>% # One row per group
  mutate(group_letters = group_data %>%
           str_replace_all("\n", "") %>%
           str_split(pattern = "") %>%
           str_unique() )
  mutate(my_data$test = NA)

for(i in seq_along (my_data$group_data)){
  my_data$test[i] <- my_data$group_letters[i][[1]] %in% letters
}


my_data <- my_data %>%
  mutate(test = stri_list2matrix(group_letters), brow = TRUE)
my_data$group_letters[1]