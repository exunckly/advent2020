# Day 2: Advent of Code 2020

# Format of data is range for no. of times a letter appears in a string, the letter, then the string


input_filename <- "data/d2_p1_test.txt"
#input_filename <- "data/d1_p1_input.txt"

initial_data <- read_csv(input_filename, col_names = FALSE)

# part 1
# Final answer is number of valid passwords

my_data <- initial_data %>%
  separate(X1, into = c("min", "max", "letter", "password")) %>%
  mutate_at("min", as.numeric) %>%
  mutate_at("max", as.numeric) %>%
  # Count matching letters, test whether within the range
  mutate(n = str_count(password, letter)) %>%
  mutate(valid = (n >= min & n <= max))

part_1 <- my_data %>%
  count(valid) %>%
  filter(valid == TRUE)

print(paste0("Number of valid passwords: ", part_1$n[1]))

  
