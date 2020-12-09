# Day 9: Advent of Code 2020
library(tidyverse)

#input_filename <- "data/d09_test.txt"
input_filename <- "data/d09_input.txt"

# Initialise
my_data <- read_csv(input_filename, col_names = FALSE)
preamble <- 25

# In Part 1 we sum two numbers, but may need to watch out for integer overflow?
# Test if any numbers in the input are larger than half of R's limit

r_max <- 9007199254740992
my_data <- my_data %>%
  mutate(overflow_check = X1 > r_max/2)

overflowed <- my_data %>%
  filter(overflow_check == TRUE) %>%
  nrow()

print(paste("Number of values that can cause an integer overflow error:", overflowed))

# Function to generate valid options from a series of numbers
# Valid options are any two of the numbers in the series added together
# Function takes n as an argument in case this is different in part 2
generate_valid <- function(my_numbers, n){
  x <- colSums(combn(my_numbers, n))
  return(list(x))
}

# Part 1
# Find the number that is not the sum of the previous x numbers in the sequence, where x = preamble
# Set up dataframe

my_data$options <- NA
my_data$valid <- NA

i <- preamble + 1
start<- i-preamble

# Come back and learn to do the assignments below using something like map
for(i in seq_along(my_data$X1)){
  if (i > preamble){
    start<- i-preamble-1
    finish <- i-1
    my_data$options[i] <- generate_valid(my_data$X1[start:finish], 2)
    my_data$valid[i] <- my_data$X1[i] %in% my_data$options[i][[1]]
  }
}

# Identify first number that is not valid
part1 <- my_data %>%
  filter(valid == FALSE) %>%
  select(X1) %>%
  slice(1) %>%
  pull()

print(paste("Part 1 answer:", part1))

# Part 2 - find a sequence of contiguous numbers in my_data$X1 that sum to the part 1 answer

# Obviously the part 1 answer itself is a sequence of 1 number, so we don't want that
# Need to search within numbers earlier in the sequence than the location of the part 1 answer
# And we want to loop through as few iterations as possible

# Search strategy: start with the location before our answer and consecutively add items moving backwards until the answer is too large
# Then move onto starting with the previous location in the sequence, and so on

my_loc <- which(my_data$valid == FALSE)[1]

i <- my_loc # this is the outer loop and is the end of the sequence, e.g. my_data$X1[j:i]
j <- my_loc # this is the inner loop and ends up being the start of the sequence
part2 <- NA

while (i > 0 & is.na(part2)){
  i <- i - 1 # start one location further back than we did last time
  j <- i
  continue_j <- TRUE
  while (j > 0 & continue_j == TRUE){ # loop through backwards until we hit the start, or the answer is too large
    j <- j - 1
    my_ans <- sum(my_data$X1[j:i])
    if(my_ans == part1){
      part2 <- max(my_data$X1[j:i]) + min(my_data$X1[j:i]) # the value requested in the question
      print(my_data$X1[j:i])
    } else if(my_ans > part1){ # No point in doing any more from here as it's already too big
      continue_j <- FALSE
    }
  }
}

print(paste("Part 2 answer:", part2))
