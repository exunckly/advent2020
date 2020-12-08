# Day 8: Advent of Code 2020
library(tidyverse)

#input_filename <- "data/d08_test.txt"
input_filename <- "data/d08_input.txt"

#Input has two columns - operator and argument
# jmp: jump to new location
# acc: change value of accumulator and move to next line
# nop: no operation - move to next line

my_data <- tibble(inst = readLines(input_filename)) %>% # Read in file
  separate(inst, into = c("op", "arg"), sep = " ") %>%
  mutate(arg = as.numeric(arg))

# Initialise variables
loc_acc <- c(1, 0) # initial values of location and accumulator
my_reg_op <- as.vector(my_data$op) # registry to hold operator values
my_reg_arg <- as.vector(my_data$arg) # registry to hold argument values

# Function to run a single instruction
# executes the instruction in loc, returns new values of loc and acc
run_instruction <- function(loc, acc, reg_op, reg_arg){
  if(reg_op[loc] == "jmp"){
    #print("jmp")
    loc = loc + reg_arg[loc] # jmp: jump to new location
  } else if(reg_op[loc] == "acc"){
    acc <- acc + reg_arg[loc] # acc: change value of accumulator and move to next line
    loc = loc + 1
  } else if(reg_op[loc] == "nop"){ # nop: no operation - move to next line
    loc = loc + 1
  }
  return (c(loc, acc))
}

# Function to check how the program exits
# There are two possible conditions:
# Part 1 exit condition: we hit an infinite loop (i.e. we reach a location twice)
#    - the instructions cannot modify reg_op or reg_arg (at least today)
# Part 2 exit condition (success!): we hit a location one higher than the last location in reg_loc

check_program_for_exit <- function(reg_op, reg_arg){ # trying to pass only small things
  loc_acc <- c(1, 0)
  reg_visit <- vector(mode = "numeric", length = length(reg_arg)) # number of times each location has been visited initializes as zeros
  while (max (reg_visit) < 2 & loc_acc[1] %in% seq_along(reg_arg)){ # test conditions for infinite loop and successful exit
    value_before <- loc_acc # Part 1 requires us to know the value of the accumuklator before the program hit the infinite loop, rather than after
    reg_visit[loc_acc[1]] = reg_visit[loc_acc[1]] + 1
    loc_acc <- run_instruction(loc_acc[1], loc_acc[2], reg_op, reg_arg)
  }
  if (max (reg_visit) >= 2){ # Test for infinite loop
    #print("Infinite loop")
    return(value_before)
  } else if (loc_acc[1] == length(reg_arg) + 1){ # Test for successful completion
    print("Success!")
    return(loc_acc)
  } else{ # Any other exit state - here in case it is needed
    print("Overshot") 
    return(loc_acc)
  }

}

# Part 1
#"Immediately before any instruction is executed a second time, what value is in the accumulator?"

print(paste0("Value in accumulator immediately before infinite loop: ", check_program_for_exit(my_reg_op, my_reg_arg)[2]))

# Part 2
# "Somewhere in the program, either a jmp is supposed to be a nop, or a nop is supposed to be a jmp.
# (No acc instructions were harmed in the corruption of this boot code.)

# The program is supposed to terminate by attempting to execute an instruction immediately after
# the last instruction in the file.
# By changing exactly one jmp or nop, you can repair the boot code and make it terminate correctly.

# Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp).
# What is the value of the accumulator after the program terminates?

# initialise variables for while loop
success <- FALSE
i <- 0

while (success == FALSE){ # Loop through reg_op, changing jmp or nop one at a time, testing for success
  i <- i + 1
  change <- 0 # check to see if an operator changed
  test_op <- my_reg_op # used to change one operator at a time
  if(my_reg_op[i] == "jmp"){
    test_op[i] <- "nop"
    change <- 1
  } else if (my_reg_op[i] == "nop"){
    test_op[i] <- "jmp"
    change <- 1
  }
  if (change == 1){ # only run the program if we actually change an instruction
    check <- check_program_for_exit(test_op, my_reg_arg)
    if (check[1] > length(my_reg_op)){
      success <- TRUE
    }
  }
}

print(paste0("Value in accumulator immediately after program terminates: ", check[2]))

# It may also be possible to do part 2 like yesterday's problem, by constructing a graph
# as there will only be so many ways of getting to the final location in one step
# and only so many ways of getting to those locations and you could see if the graph is
# connected when you change any one of the jmp or nop instructions.
# But this is for another day...