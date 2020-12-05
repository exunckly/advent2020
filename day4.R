# Day 4: Advent of Code 2020
library(tidyverse)

#input_filename <- "data/d4_test.txt"
#input_filename <- "data/d4_p2_test.txt"
input_filename <- "data/d4_input.txt"

initial_string <- read_file(input_filename)

# Clean up the initial string of passport information
# We want to replace all single newlines that appear within records with spaces so that they are one row per observation (passport)
# Records are currently demarked with a double newline; we want this to be a single newline
# As single newline appears within double newline, substitute double newline out before manipulating the single newlines

processed_string <- str_replace_all(initial_string, "\\\n\\\n", "my_separator")
processed_string <- str_replace_all(processed_string, "\\\n", " ")
processed_string <- str_replace_all(processed_string, "my_separator", "\\\n")

# Process into a dataframe with one row per passport

initial_data <- read_csv(processed_string, col_names = FALSE)
initial_data$my_id <- 1:nrow(initial_data) # Generate a unique ID for each entry so that the separating and pivoting works 

new_data <- initial_data %>%
  separate_rows(X1, sep = " ") %>% # one row per passport per field
  separate(X1, c("key", "value"), sep = ":") %>% # split field names and values
  pivot_wider(names_from = key, values_from = value)  # one row per passport, fields as column names; empty will be NA

# Part 1

# Check if all fields are present (i.e, the entries in the dataframe are not NA)
new_data$no_na <- new_data %>%
  is.na  %>%
  rowSums

# It is valid if all other fields are present and just the cid field is NA
new_data <- new_data %>%
  mutate (valid_pt1 = no_na == 0 | (no_na == 1 & is.na(cid)))

# Count number of valid passports
part_1 <- new_data %>%
  count(valid_pt1) %>%
  filter(valid_pt1 == TRUE)

print(paste0("Part 1 - number of valid passports: ", part_1$n[1]))


# Part 2 - apply stricter criteria to passports
# As noted above, if the field has no value it will already be NA

# Clean  fields - set numeric fields to numeric and split out height values and units
new_data <- new_data %>%
  mutate(eyr = as.numeric(eyr)) %>%
  mutate(byr = as.numeric(byr)) %>%
  mutate(iyr = as.numeric(iyr)) %>%
  mutate(hgt_val = as.numeric(str_sub(hgt, 1, -3))) %>%
  mutate(hgt_units = str_sub(hgt, start = -2))

# Set the relatively complicated validation criteria
valid_ecl <-  c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
valid_hgt <- "[0-9]+cm|[0-9]+in" #hgt (Height) - a number followed by either cm or in:
valid_hcl <- "#([0-9a-f]){6}" #hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.

# Perform validation
checked_data <- new_data %>%
  filter(byr >= 1920 & byr <= 2002) %>% # byr (Birth Year) - four digits; at least 1920 and at most 2002.
  filter(iyr >= 2010 & iyr <= 2020) %>% # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  filter(eyr >= 2020 & eyr <= 2030) %>% # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  filter(ecl %in% valid_ecl) %>% # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  filter(str_detect(hcl, valid_hcl)) %>% # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  filter(nchar(pid) == 9) %>% # pid (Passport ID) - a nine-digit number, including leading zeroes.
  filter(!is.na(as.numeric(pid))) %>% 
  filter(str_detect(hgt, valid_hgt)) %>% #hgt (Height) - a number followed by either cm or in:
  filter((hgt_val >= 150 & hgt_val <= 193 & hgt_units == "cm") | # If cm, the number must be at least 150 and at most 193.
           (hgt_val >= 59 & hgt_val <= 76 & hgt_units == "in")) %>% # If in, the number must be at least 59 and at most 76.
  mutate(valid_pt2 = TRUE) # Allows us to join with original dataset so we have thrown nothing away

# Join back up with original data (so we have thrown nothign away by using filter)
part2_data <- left_join(new_data, select(checked_data, my_id, hgt_val, hgt_units, valid_pt2), by = "my_id")

# Count number of valid passports
part_2 <- part2_data %>%
  count(valid_pt2) %>%
  filter(valid_pt2 == TRUE)

print(paste0("Part 2 - number of valid passports: ", part_2$n[1]))