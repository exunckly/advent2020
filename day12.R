# Day 12: Advent of Code 2020
library(tidyverse)

# Ship can go north, south, east or west, regardless of which way it is currently facing
# Ship can also go forward in the direction it is currently facing
# Ship can turn left or right to face in a different direction

# Represent the direction the ship is facing from 0 (North) to 359
# Do arithmetic and modulo 360 it to get the new direction, right is +ve and left is -ve

#my_data <- read_csv("data/d12_test.txt", col_names = FALSE)
my_data <- read_csv("data/d12_input.txt")
my_data <- my_data %>%
  mutate(instruction = substr(X1, 1,1)) %>%
  mutate(magnitude = parse_number(X1))

# Store historical positions and directions in case we need them for part 2

my_data <- my_data %>%
  mutate(dir = NA,
         xpos = NA,
         ypos = NA) %>%
  add_row(dir = 90, xpos = 0, ypos = 0, .before = 1) # ship starts out facing East, choose (0,0) as origin

move_ship <- function(instruction, magnitude, initial_dir){ # assumes input of L or R
  x_move <- 0
  y_move <- 0
  new_dir <- initial_dir
  
  if(instruction == "F"){
    instruction <- case_when(
      initial_dir == 0 ~ "N", 
      initial_dir == 90 ~ "E",
      initial_dir == 180 ~ "S",
      initial_dir == 270 ~ "W"
    )
  }
  
  if(instruction == "N"){
    y_move <- y_move + magnitude
  } else if(instruction == "S"){
    y_move <- y_move - magnitude
  } else if(instruction == "E"){
    x_move <- x_move + magnitude
  } else if(instruction == "W"){
    x_move <- x_move - magnitude
  } else if(instruction == "R"){
    new_dir <- (initial_dir + magnitude) %% 360
  } else if(instruction == "L"){
    new_dir <- (initial_dir - magnitude) %% 360    
  }
  return(c(x_move, y_move, new_dir))
}


my_answer <- my_data

for(i in seq_along(my_answer$dir)){
  if (i > 1){
    z <- move_ship(my_answer$instruction[i], my_answer$magnitude[i], my_answer$dir[i-1])
    my_answer$xpos[i] <- my_answer$xpos[i-1] + z[1]
    my_answer$ypos[i] <- my_answer$ypos[i-1] + z[2]
    my_answer$dir[i] <- z[3]
  }
}

# Part 1 solution
final_loc <- tail(my_answer,1)

part1_ans <- abs(final_loc$xpos[1]) + abs(final_loc$ypos[1])
part1_ans

