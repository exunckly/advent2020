# advent2020

# Things I have learned from looking at others' solutions
(I have not updated my own original solutions to reflect these, but may use them going forwards)

readr::parse_number - extracts number from string - I didn't know about this at all
https://www.rdocumentation.org/packages/readr/versions/1.3.1/topics/parse_number

tidyr::separate_rows can have the separator defined, e.g, separate_rows(my_data, sep = "\n\n")
This means that I didn't need to parse out the double newlines from one of the input files (which also contained single newlines within the data)

tibble::rowid_to_column - does what it says on the tin and works in a pipe.
Much nicer than what I did: initial_data$my_id <- 1:nrow(initial_data)

Rather than using expand.grid to find which two numbers in a set sum to a total, I could have used a neat shortcut:
my_data %>% filter ((total - val) *in* val)

In dplyr::filter
* Concept of multiple conditions with & in a single statement rather than one row each (perhaps harded to debug, however?)
* filter(between(iyr, 2010, 2020) & other conditions) # instead of one line ecah, e.g. filter(iyr >= 2010 & iyr <= 2020) 

Piping and mutating
* You can put pipes inside pipes, until the thing looks like the Mull of Kintyre overdub session...
* You can also set several variables within the same mutate statement (but again this is maybe less easy to debug and it's harder to comment stuff out by putting # before various different %>%s?)
What I had:
my_data <- read_csv(input_filename, col_names = FALSE) %>%
  mutate(binary = str_replace_all(X1, "F|L", "0")) %>%
  mutate(binary = str_replace_all(binary, "B|R", "1")) %>%
  mutate(row = strtoi(str_sub(binary, 1, 7), base = 2)) %>%
  mutate(column = strtoi(str_sub(binary, 8, 10), base = 2)) %>%
  mutate(seat_id = row * 8 + column)

What is possible:
my_data <- read_csv(input_filename, col_names = FALSE) %>%
  mutate(binary = str_replace_all(X1, "F|L", "0") %>% 
                  str_replace_all("B|R", "1"), # This is definitely nicer as there is no change of variable
        row = str_sub(binary, 1, 7) %>% strtoi (base = 2),
        column = str_sub(binary, 8, 10) %>% strtoi (base = 2),
        seat_id = row * 8 + column)
        
Trees problem (day 3) - it didn't even enter my head to read the landscape into a data frame and work on it in the dataframe by generating sequences for the moves. This wouldn't have been elegant if the route ever involved going both up and down (as previous ouzzles have) but is worth bearing in mind rather than writing a while loop

If you ever use read.table and your data has # in it, be sure to define the comment character as "" (didn't happen to me as I used read_csv or readLines so far, but something to watch out for in the future)

Passport validation - it didn't enter my head to keep it at one row per passport per field, and check whether the individual fields were valid before pivoting wide (I pivoted wide early, filtered then joined back up with the original dataset)







                       
