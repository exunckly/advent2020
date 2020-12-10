# Day 10: Advent of Code 2020
library(tidyverse)

#input_filename <- "data/d10_test1.txt"
#input_filename <- "data/d10_test2.txt"
input_filename <- "data/d10_input.txt"

my_data <- read_csv(input_filename, col_names = FALSE)
biggest_adapter <- max(my_data$X1)

# Part 1
my_data <- my_data %>%
  add_row(X1 = biggest_adapter + 3) %>%
  add_row(X1 = 0) %>%
  arrange(X1) 

# Answer is the number of diffs of 1 * number of diffs of 3
part1 <- diff(my_data$X1)
print(paste("Answer", sum(part1 %in% 1) * sum(part1 %in% 3)))

# Part 2

# How many ways are there of removing numbers from the sequence so that the maximum diff is still 3 
# Currently here are ONLY diffs of 1 or 3 (no diffs of 2): 
unique(part1)

# so we only need to consider the number of combinations that a consecutive string of diffs of 1 can have
# As the gaps on the outer diffs are 3, the outer numbers always need to be there,
# so the question becomes what combination of numbers can we remove from in between for each series of 1s
# Then we multiply the combinations for each series of 1s together to get all of the permutations

# What is the longest series of 1s in my input?

my_run_lengths <- tibble(values = rle(part1)$values, lengths = rle(part1)$lengths)
# rle() = run length equivalent - represents a sequence as each value, then the number of times that value appears consecutively,
# like how images used to be encoded in the 1980s (pixel colour, number of pixels of that colour)

longest_series <- my_run_lengths %>%
  filter(values == 1) %>%
  max(lengths)

# In my input, the longest series of 1s is 5 long, so if I work out the number of combinations
# for run lengths of 1-5 then it should work OK
# NB: like the fenceposts problem, 2 gaps = 3 numbers etc.

# Come back and work out how to calculate the combinations computationally another time
my_combs <- tibble(lengths = c(1, 2, 3, 4, 5), combs = c(1, 2, 4, 7, 10))

part2 <- my_run_lengths %>%
  left_join (my_combs, by = "lengths") %>%
  filter (values == 1) %>%
  select(combs) %>%
  prod()

print(format(part2, scientific = F))
