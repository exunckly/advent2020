# Day 15: Advent of Code 2020

# In this game, the players take turns saying numbers. They begin by taking turns reading from a list of starting numbers (your puzzle input).
# Then, each turn consists of considering the most recently spoken number:
#   
#   - If that was the first time the number has been spoken, the current player says 0.
#   - Otherwise, the number had been spoken before; the current player announces how many turns apart
#     the number is from when it was previously spoken.
# 
# So, after the starting numbers, each turn results in that player speaking aloud
# either 0 (if the last number is new) or an age (if the last number is a repeat).

# Part 1

# Given your starting numbers, what will be the 2020th number spoken?

test0 <- c(0,3,6) #   436
test1 <- c(1,3,2) #   1
test2 <- c(2,1,3) #   10
test3 <- c(1,2,3) #   27
test4 <- c(2,3,1) #   78
test5 <- c(3,2,1) #   438
test6 <- c(3,1,2) #   1836
my_input <- c(12,1,16,3,11,0)

# Initialise variables
my_data <- my_input

# Compute part 1 (naive solution)

calc_number1 <- function(initial_nos, my_length){
  x <- as.numeric(rep(NA, my_length))
  data_length <- length(initial_nos)
  x[1:data_length] <- initial_nos
  
  for(i in seq_along(x)){
    if (i %% 1000 == 0) print(paste("i =", i))
    if(is.na(x[i])){
      i_minus_2 <- i-2
      if(x[i-1] %in% x[1:i_minus_2]){ # number has been been spoken before
        x[i] <- i - 1 - max(which(x[1:i_minus_2] %in% x[i-1]))
      } else { # number has not been spoken before
        x[i] <- 0
      }
    }
  }
  return(x[i])
}
part1 <- calc_number1(my_data, 2020)
part1


# Part 2

#part2 <- calc_number1(my_data, 30000000) # unsurprisingly, this was too slow - it takes longer and longer to do the %in% part

# We actually only need to keep track of which numbers have occurred and when they were most recently spoken
# Setup assumes that input is of unique numbers (which is always the case here)


# Implementation 2 - keep track of only the most recent version of each number rather than all of them
calc_number2 <- function(initial_nos, my_length){ # Less naive function but still slow
  data_length <- length(initial_nos)
  numbers_spoken <- initial_nos # would be faster to dim it up front so it doesn't get copied each time
  recent_spoken <- seq_along(numbers_spoken) # would be faster to dim it up front so it doesn't get copied each time
  x <- numbers_spoken[data_length]
  
  for(i in 1:my_length){
   # print(paste("i =", i))
    if (i > data_length){
      if(i %% 10000 == 0) print(i)
      if(x %in% numbers_spoken){
        # Find location of this number in our vector of numbers spoken
        numloc <- (which(numbers_spoken %in% x))
        # Find difference between now and most recent location
        x <- i - 1 - recent_spoken[numloc] # this may be zero
        # Update our note of when this number was most recently spoken
        recent_spoken[numloc] <- i - 1
      } else{
        # Add this number to our vectors of numbers spoken and their locations
        nextpos <- length(numbers_spoken) + 1
        numbers_spoken[nextpos] <- x
        recent_spoken[nextpos] <- i - 1
        #print(numbers_spoken)
        #print(recent_spoken)
        # Note in our main vector that this was the first time the number was spoken
        x <- 0
      }
      #print(x)
    }
  }
  return(x)
}

# Initialise variables
# my_data <- my_input
#calc_number2(my_data, 300000000) # also takes a fairly long time

# Implementation 3 - hashtable

# Environment hash functions
# From https://www.r-bloggers.com/2019/01/hash-me-if-you-can/
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")


calc_number3 <- function(initial_nos, my_length){ # Version using hash table
  data_length <- length(initial_nos)
  my_key <- as.character(initial_nos)
  recent_spoken <- seq_along(initial_nos)
  x <- initial_nos[data_length] # most recent number
  
  hash <- new.env(hash = TRUE, parent = emptyenv(), size = my_length + 1)
  assign_hash(my_key, recent_spoken, hash) # key, value, hash

  for(i in 1:my_length){
    # print(paste("i =", i))
    if (i > data_length){
      if(i %% 10000 == 0) print(i)
      #print(paste("x =", x))
      if(exists_hash(as.character(x), hash)){
        # Find location of this number in our hashtable
        # Find difference between now and most recent location
        
        #print(get_hash(as.character(x), hash))
        new_x <- i - 1 - get_hash(as.character(x), hash) # this may be zero
        # Update the hash with the  new most recent time the number was spoken
       # print(paste("i =", i, "x =", x))
      #  print(get_hash(ls(hash), hash))
        hash[[as.character(x)]] <- i - 1
        x <- new_x
      } else{
        # Add this number to our vectors of numbers spoken and their locations
        hash[[as.character(x)]] <- i - 1
        # nextpos <- length(numbers_spoken) + 1
        # numbers_spoken[nextpos] <- x
        # recent_spoken[nextpos] <- i - 1

        # Note that this was the first time the number was spoken
        x <- 0
      }
    #  print(paste("i =", i, "x =", x))
    }
  }
  return(x)
}



# Initialise variables
#my_data <- my_input
#initial_nos <- my_data # placeholder
# a <- calc_number3(my_data, 30000000) # This still took too long

# Implementation 4 - the indices of the vector are the numbers spoken, and the value of the vector is when it was last spoken
# This means we never have to find the number, we just look it up by index


calc_number4 <- function(initial_nos, my_length){ # Less naive function but still slow
  track_vector <- as.numeric(rep(NA, my_length+1))
  for(i in seq_along(initial_nos)){
    track_vector[initial_nos[i]+1] <- i #  Deal with offset by 1, as zero is possible
  }
  
  data_length <- length(initial_nos)
  x <- initial_nos[data_length]
  
  for(i in 1:my_length){
    if (i > data_length){
      if(i %% 1000000 == 0) print(i)
      if(!is.na(track_vector[x+1])){
        # Find difference between now and most recent location
        new_x <- i - 1 - track_vector[x+1]
        # Update our note of when this number was most recently spoken
        track_vector[x+1] <- i - 1
        x <- new_x
      } else{
        # Add this number to our vectors of numbers spoken and their locations
        track_vector[x+1] <- i - 1
        # Note that this was the first time the number was spoken
        x <- 0
      }
    }
  }
  return(x)
}

calc_number4(my_data, 30000000)



