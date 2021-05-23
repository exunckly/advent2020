# Day 12: Advent of Code 2020
library(tidyverse)

# Ship can go north, south, east or west, regardless of which way it is currently facing
# Ship can also go forward in the direction it is currently facing
# Ship can turn left or right to face in a different direction

# Represent the direction the ship is facing from 0 (North) to 359
# Do arithmetic and modulo 360 it to get the new direction, right is +ve and left is -ve

#my_data <- read_csv("data/d12_test.txt", col_names = FALSE)
my_data <- read_csv("data/d12_input.txt", col_names = FALSE)
my_data <- my_data %>%
  mutate(instruction = substr(X1, 1,1)) %>%
  mutate(magnitude = parse_number(X1))

# Store historical positions and directions in case we need them for part 2

my_data1 <- my_data %>%
  mutate(dir = NA,
         xpos = NA,
         ypos = NA) %>%
  add_row(dir = 90, xpos = 0, ypos = 0, .before = 1) # ship starts out facing East, choose (0,0) as origin

move_ship_part1 <- function(instruction, magnitude, initial_dir){ # assumes input of L or R
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


part1_answer <- my_data1

for(i in seq_along(part1_answer$dir)){
  if (i > 1){
    z <- move_ship_part1(part1_answer$instruction[i], part1_answer$magnitude[i], part1_answer$dir[i-1])
    part1_answer$xpos[i] <- part1_answer$xpos[i-1] + z[1]
    part1_answer$ypos[i] <- part1_answer$ypos[i-1] + z[2]
    part1_answer$dir[i] <- z[3]
  }
}

# Part 1 solution
final_loc1 <- tail(part1_answer,1)

part1 <- abs(final_loc1$xpos[1]) + abs(final_loc1$ypos[1])
part1

# Part 2 - same instructions, different meaning

# Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:

# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal to the given value.

# The waypoint starts 10 units east and 1 unit north relative to the ship.
# The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.

# Final answer is Manahtten distance from ship to its starting position

my_data2 <- my_data %>%
  mutate(s_xpos = NA,
         s_ypos = NA,
         w_r_xpos = NA,
         w_r_ypos = NA) %>%
  add_row(s_xpos = 0, s_ypos = 0, w_r_xpos = 10, w_r_ypos = 1, .before = 1) # Choose (0,0) as origin

move_ship_part2 <- function(instruction, magnitude, relpos){ # assumes input of L or R
  # relpos is relative position of waypoint to ship, calculated outside the function
  x_move <- 0
  y_move <- 0
  ship_moves <- 0
  
  if(instruction == "L" | instruction == "R"){
    # rotation matrix goes anticlockwise from the x-axis and works in radians
    theta = magnitude * ifelse(instruction == "L", 1, -1) * (2*pi/360)
    rotmat <- round(array(c(cos(theta), sin(theta), -sin(theta), cos(theta)), dim = c(2,2)), digits = 5) # round otherwise we get non-zero results
    new_relpos <- rotmat %*% relpos
    x_move <- new_relpos[1] - relpos[1]
    y_move <- new_relpos[2] - relpos[2]
  }
  
  
  if(instruction == "F"){
    x_move <- x_move + magnitude*relpos[1]
    y_move <- y_move + magnitude*relpos[2]
    ship_moves <- 1
  }
  
  if(instruction == "N"){
    y_move <- y_move + magnitude
  } else if(instruction == "S"){
    y_move <- y_move - magnitude
  } else if(instruction == "E"){
    x_move <- x_move + magnitude
  } else if(instruction == "W"){
    x_move <- x_move - magnitude
  } 
  return(c(x_move, y_move, ship_moves))
}

part2_answer <- my_data2

for(i in seq_along(part2_answer$s_xpos)){
  if (i > 1){
    z <- move_ship_part2(part2_answer$instruction[i],
                         part2_answer$magnitude[i],
                         array(c(part2_answer$w_r_xpos[i-1], part2_answer$w_r_ypos[i-1]), dim = c(2,1)))
    # Either the ship moves or the waypoint moves relative to the ship
    part2_answer$s_xpos[i] = ifelse(z[3] == 0, part2_answer$s_xpos[i-1], part2_answer$s_xpos[i-1] + z[1])
    part2_answer$s_ypos[i] = ifelse(z[3] == 0, part2_answer$s_ypos[i-1], part2_answer$s_ypos[i-1] + z[2])
    part2_answer$w_r_xpos[i] = ifelse(z[3] == 0, part2_answer$w_r_xpos[i-1] + z[1], part2_answer$w_r_xpos[i-1])
    part2_answer$w_r_ypos[i] = ifelse(z[3] == 0, part2_answer$w_r_ypos[i-1] + z[2], part2_answer$w_r_ypos[i-1])
  }
}

final_loc2 <- tail(part2_answer,1)

part2 <- abs(final_loc2$s_xpos[1]) + abs(final_loc2$s_ypos[1])
part2

