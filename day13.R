# Day 13: Advent of Code 2020
library(tidyverse)
library(FRACTION)
library(primes)

# I suspect there is some clever modulo way to do this, but here's how I got on myself

#input_filename <- "data/d13_test.txt"
input_filename <- "data/d13_input.txt"
part2_test <- (c("7,13,x,x,59,x,31,19", "17,x,13,19", "67,7,59,61", "67,x,7,59,61" , "67,7,x,59,61", "1789,37,47,1889"))
my_test <- 0

initial_string <- read_lines(input_filename)

my_time <- as.numeric(initial_string[[1]])
bus_times <- initial_string[2] %>%
  str_replace_all(",x", "")
bus_times <- as.numeric(str_split(bus_times, ",")[[1]])

# Part 1
# Work out next bus to leave using modulo

wait_time <- min(bus_times - (my_time %% bus_times))
next_bus <- bus_times[which.min(bus_times - (my_time %% bus_times))]

part1 <- wait_time * next_bus

# Part 2 - now x-position in vector represents the number of mins past a time, target_t, that we aim for the bus to leave
# Here T = period of bus and offset = time past target_t = 0

if (my_test == 0){
  bus_delays <- tibble(T = initial_string[[2]]) 
} else{
  bus_delays <- tibble(T = part2_test[my_test])
}

# Set up dataset so we can increment loop in steps of the largest bus time period, T
# instead of whatever bus happens to arrive at t = 0
bus_delays_table <- bus_delays %>%
  separate_rows(T, sep = ",") %>%
  mutate(offset = row_number() - 1) %>%
  filter(T != "x") %>%
  mutate(T = as.numeric(T)) %>%
  mutate(offset = as.numeric(offset)) %>%
  mutate(target = (T - offset) %% T) %>% # remainders we are aiming for when dividing the time t by each but period T
  arrange(desc(T)) %>% # Shift results so we can work wuth a setpsize of the largets bus period
  mutate(newoffset = offset - offset[1]) %>%
  mutate(newoffset = newoffset %% T) %>%
  mutate(newtarget = (T - newoffset) %% T) %>%
  mutate(is_prime = primes::is_prime(T)) # all periods are prime numbers in my actual dataset


myoffset<- bus_delays_table$newoffset
myT <- bus_delays_table$T
target <- bus_delays_table$newtarget

# Notice that several buses now have the same offset (zero) - in my dataset at least
# So can up the stepsize to the lowest common multiple of the Ts of these buses

# As the Ts are all prime in my real dataset could multiply them together to get the lowest common multiple
# but will  instead use a function so that the code works with non-prime periods as well

small_dataset <- bus_delays_table %>%
  filter(newoffset == 0) # by design there will always be at least one row

# Finds lowest common multiple of a vector of values
my_lcm <- function(x){
  i <- 1
  current_lcm <- x[1]
  while(i < length(x)){
    current_lcm <- (current_lcm * x[i+1])/primes::gcd(x[i], x[i+1])
    i <- i + 1
  }
  return(current_lcm)
}

# Find the part 2 answer
stepsize <- my_lcm(small_dataset$T)
i <- stepsize
repeat{
  if(isTRUE(all.equal(i %% myT, target))) break
  i <- i + stepsize
}
part2 <- i - bus_delays_table$offset[1] # as we shifted to take advantage of large stepsizes
print(part2)