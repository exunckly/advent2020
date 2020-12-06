# Day 3: Advent of Code 2020

#input_filename <- "data/d3_p1_test.txt"
input_filename <- "data/d3_p1_input.txt"
input_slopes <- "data/d3_slopes.txt"

# Read data into a vector
# First index is row, 2nd index is column, numbered from top left
initial_data <- read_lines(input_filename)
list_data <- strsplit(initial_data, "")
hillside <- t(do.call(cbind, list_data))

# Problem seems to indicate we will need to traverse using different 'slopes' so write it as a function
traverse <- function(hillside, my_row, my_col, right, down, tree){
  no_trees <- 0
  height <- nrow(hillside)
  width <- ncol(hillside)
  
  while (my_row <= height){ # We end when y position is equal to the height of the mountain (number of rows in my_data)
    if(hillside[my_row, my_col] == tree){
      no_trees <- no_trees + 1
    }
    my_col <- (my_col + right)
    if (my_col != width){
      my_col = my_col %% width # Use modulus to wrap (bearing in mind that R arrays start at 1)
    }
    my_row <- my_row + down
  }
  return(no_trees)
}

# Part1

start_row <- 1
start_col <- 1
right_step <- 3
down_step <- 1
tree <- "#"

print(paste0("Part 1 - number of trees: ", traverse(hillside, start_row, start_col, right_step, down_step, tree)))

# Part2
# Final answer is number of trees on each slope multiplied together
# Read in the slopes and loop through them

my_slopes <- read_csv(input_slopes, col_names = FALSE) %>%
  mutate_all(str_extract, "[0-9]") %>%
  mutate_if(is.character, as.numeric) %>%
  rename(right = X1) %>%
  rename(down = X2) %>%
  mutate(trees = NA)

# Come back and learn how to do the part below in map one day...
for (i in seq_along(my_slopes$right)){
  my_slopes$trees[i] = traverse(hillside, start_row, start_col, my_slopes$right[i], my_slopes$down[i], tree)
}

print(paste0("Part 2 - number of trees: ",prod(my_slopes$trees)))

