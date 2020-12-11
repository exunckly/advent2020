# Day 11: Advent of Code 2020
library(tidyverse)

#input_filename <- "data/d11_test.txt"
input_filename <- "data/d11_input.txt"

# Prepare datasets

# Function to convert lines to numerical matrix
lines_to_num_mat <- function(my_lines){
  my_list <- strsplit(my_lines, "")
  my_list <- lapply(my_list, function (i) as.numeric(i))
  my_mat <- t(do.call(cbind, my_list))
  return(my_mat)
}

# state_mat is the initial state of our matrix
# Initially, L = empty, # = occupied, . = always empty (does not change state) but can influence state of neighbours
state_lines <- read_lines(input_filename) %>%
  str_replace_all("L", "0") %>%
  str_replace_all("#", "1") %>%
  str_replace_all("\\.", "0")
state_mat <- lines_to_num_mat(state_lines)

# This one is the floor locations which never change
# The idea is to calculate new state then multiply by a grid where empty = 0 and potentially occupied = 1 to flip them back again
zero_lines <- read_lines(input_filename) %>%
  str_replace_all("L", "1") %>%
  str_replace_all("#", "1") %>%
  str_replace_all("\\.", "0")
zero_mat <- lines_to_num_mat(zero_lines)

height <- nrow(state_mat)
width <- ncol(state_mat)

# Calculate new state then multiply by a grid where empty = 0 and potentially occupied = 1 to flip the always empty locations back again

# Calculate state by shifting the grid in each of 8 directions and adding them together

# for testing
#state_mat <- (state_mat + 1) * c(1:10)
i <- 0

repeat {
  dirN <- rbind(FALSE, state_mat[-height,])
  dirS <- rbind(state_mat[-1,], FALSE)
  dirW <- cbind(FALSE, state_mat[,-width])
  dirE <- cbind(state_mat[,-1], FALSE)
  dirNW <- rbind(FALSE, dirW[-height,])
  dirSW <- rbind(dirW[-1,], FALSE)
  dirNE <- rbind(FALSE, dirE[-height,])
  dirSE <- rbind(dirE[-1,], FALSE)

  change_state <- dirN + dirS + dirW + dirE + dirNW + dirSW + dirNE + dirSE
  
  new_state <- (ifelse( (state_mat == 0 & change_state == 0) | (state_mat == 1 & change_state < 4), 1, 0)) * zero_mat
  i <- i + 1
  print(i)
  if (identical(new_state, state_mat)) break
  state_mat <- new_state
}

#Part1
occupied <- sum(new_state)
print(occupied)
