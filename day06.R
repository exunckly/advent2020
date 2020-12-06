# Day 6: Advent of Code 2020
library(tidyverse)

#input_filename <- "data/d06_test.txt"
input_filename <- "data/d06_input.txt"

# Dataframe of one row per group per passenger per response
my_data <- tibble(response = read_file(input_filename)) %>%
  separate_rows(response, sep = "\n\n") %>% # Now one row per group
  rowid_to_column("group_id") %>% 
  mutate(n_pass_in_group = str_count(response, "\n") + 1) %>% # no. passengers in each group (needed for part 2 - could also do later in similar way to counting responses)
  separate_rows(response, sep = "\n") %>% # Now one row per group per passenger
  rowid_to_column("passenger_id") %>% 
  separate_rows(response, sep = "") %>% # Now one row per group per passenger per response
  filter(response %in% letters) #  Lower case letter responses only
  
# Part 1 - we only care about responses at the group level, so remove passenger-level information
# Generate dataframe with one row per group per unique response
part1_data <- my_data %>%
  select(-passenger_id) %>%
  distinct()

# If we needed to know the sum in each individual group, we could: %>% group_by(group_id) %>% count() 
# But we only need the sum across the groups, which is the number of rows in the table

print(paste0("Part 1 - sum of counts: ", nrow(part1_data)))

# Part 2 - now we want to know at a group level which questions EVERYONE in the group answered yes to
# Add a column that counts number with each response in each group, then compare with the number of passengers in the group
part2_data <- my_data %>%
  add_count(group_id, response) %>% # n = number with each response in each group
  select(-passenger_id) %>%
  distinct() %>% # one row per group per response
  filter (n == n_pass_in_group) # compare with number of passengers in the group

print(paste0("Part 2 - sum of counts: ", nrow(part2_data)))


# The code below was my original solution to part 2, before I discovered add_count
# The above is more straightforward as you keep working in the same dataframe
# and don't need to join back up again to bring in the number of passengers in each group

# part2_data <- my_data %>%
#   count(group_id, response) %>% # note: this calls group_by() at the start and ungroup() at the end
#   left_join(my_data %>% select (group_id, n_pass_in_group) %>% distinct(),
#             by = "group_id") %>% # Bring in the number of passengers in each group
#   filter (n == n_pass_in_group) # Keep rows where n in group answering Q = n in group
