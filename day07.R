# Day 7: Advent of Code 2020

library(tidyverse)
library(igraph)

#input_filename <- "data/d07_p1_test.txt"
#input_filename <- "data/d07_p2_test.txt"
input_filename <- "data/d07_input.txt"

# Prepare dataset containing 'from', 'to' and 'weight', ready for igraph
# from: outer bag, to: inner bag, weight: number of inner bags
initial_input <- read_file(input_filename)
processed_input <- initial_input %>% # Standardize plurals and punctuation
  str_replace_all("\\.", "") %>%
  str_replace_all("bags", "bag")

my_data <-tibble(from = processed_input) %>%
  separate_rows(from, sep = "\\\n") %>%
  separate(col = from, into = c("from", "to"), sep = " contain ") %>%
  separate_rows(to, sep = ", ") %>%
  mutate(weight = parse_number(to)) %>%
  mutate(to = str_replace_all(to, paste0(as.character(weight), " "), "")) %>%
  mutate(weight = ifelse(to == "no other bag", 0, weight)) # igraph appeared unsure about NA

# Put dataset into igraph
my_graph <- graph_from_data_frame(my_data, directed=TRUE)

# Define my bag
my_bag <- "shiny gold bag"

# Part 1
paths_to_my_bag <- all_simple_paths(my_graph, my_bag, mode = "in")

# Extract unique bag colours from paths_to_my_bag
if(length(paths_to_my_bag) != 0) { # To account for case of when there are no outer bags (e.g. 2nd test dataset)
  outer_bags <- unlist(paths_to_my_bag) %>%
    names() %>%
    tibble(bag = .) %>%
    filter(bag != my_bag) %>%
    distinct()
} else {
  outer_bags = 0
}

print(paste0("Part 1 - number of bag colours containing at least 1 ", my_bag, " is ", nrow(outer_bags)))

# Part 2
paths_from_my_bag <- all_simple_paths(my_graph, my_bag, mode = "out")

# And here I could not work out how to get the edge weights out of the paths_from_my_bag variable :-(

# So I found some code to extract the paths to a list of character vectors
l <- unlist(lapply(V(my_graph) , function(x) all_simple_paths(my_graph, from=my_bag, mode = "out")), recursive = F)
paths <- lapply(1:length(l), function(x) as_ids(l[[x]]))

# And I found some code to extract a matrix of consecutive pairs from a vector
my_consecutive_pairs <- function(x){
  y <- cbind(x[-length(x)], x[-1])
  return(y)
}

# Then bashed this out to get the total number of bags
inner_bags <- tibble(paths) %>% # Now one row per path
  rowwise() %>%  # So that the next line works
  mutate(my_pairs = list(my_consecutive_pairs(paths)))%>%
  mutate(paths_string = paste(paths, collapse = ",")) %>% # Paths are now in a format I can cope with
  ungroup() %>% # Turns off rowwise
  filter(!str_detect(paths_string, "no other bag")) %>% # Avoid double counting the last bag in each path
  unnest(my_pairs) %>% # Now one row per path per consecutive pair of vertices on that path
  mutate(from = my_pairs[,1],
         to = my_pairs[,2]) %>%
  select(-my_pairs, -paths) %>% # remove helper columns
  distinct() %>% # gets rid of repetition caused by paths_from_my_bag containing all possible paths from my_bag to any other bag
                 # giving a lot of repeated pairs, e.g. of initial step from my_bag to next bag down
  left_join(my_data, by = c("from", "to")) %>% # Bring in weights from full dataset
  group_by(paths_string) %>%
  summarise(stage = prod(weight)) # for each path to a distinct bag, the total number of bags is the product of the weights of the consecutive pairs of vertices

print(paste0("Part 2 - number of bags inside my ", my_bag, " is ", sum(inner_bags$stage)))

# I'll come back and actually learn how to write a recursive function some day...



