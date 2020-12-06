# advent2020

# Day 6: things I learned while working on the puzzle
Grouping and summarising
* count(x,y) is equivalent to group(x,y) %>% tally() %>% ungroup - IT UNGROUPS AT THE END< YAY!
* add_count(x,y) adds a column to your existing table, meaning that you don't have to left_join back up at the end

# All days: things I learned from looking at others' solutions
(I have not updated my own original solutions to reflect these, but may use them going forwards)


Grouping and summarising
* As distinct can take multiple arguments, we don't have to drop other columns before using it, e.g.
```R
%>% distinct(b,c)
```
instead of
```R
%>% select (-a) %>% distinct()
```

Split string into *vector* rather than into list by grabbing the first element of the list up front:
```R
a <- str_split("abc", "")[[1]]
```

intersect may be helpful in the future:
```R
intersect(c("a", "b", "c"), c("b", "c", "d"))
[1] "b" "c"
```

readr::parse_number - extracts number from string - I didn't know about this at all

tidyr::separate_rows can be useful a lot eaerlier than I had realised int he process e.g, 
`separate_rows(my_data, sep = "\n\n")`
This means that I didn't need to parse out the double newlines from one of the input files (which also contained single newlines within the data)

Allocating unique IDs to rows (e.g. before separating them further)
* tibble::rowid_to_column - does what it says on the tin (much nicer than what I did: initial_data$my_id <- 1:nrow(initial_data))
* If you want unique IDs within groups, then dplyr::row_number() respects grouping - but does also rank the answers



In dplyr::filter
* Concept of multiple conditions with & in a single statement rather than one row each (perhaps harder to debug, however?)
* you can use a custom function in filter, e.g.
```R
check_ecl <- function(x) {
  x %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
}

my_data <- my_data %>%
  filter(check_ecl(ecl))
```

Piping and mutating
* You can put pipes inside pipes, until the thing looks like the Mull of Kintyre overdub session...
* mutate multiple things at once, in the same way, e.e.
```R
mutate_at(c("byr", "iyr", "eyr"), as.numeric) 
```
* You can also set several variables within the same mutate statement (but again this is maybe less easy to debug and it's harder to comment stuff out by putting # before various different %>%s?)
What I had:
```R
my_data <- read_csv(input_filename, col_names = FALSE) %>%
  mutate(binary = str_replace_all(X1, "F|L", "0")) %>%
  mutate(binary = str_replace_all(binary, "B|R", "1")) %>%
  mutate(row = strtoi(str_sub(binary, 1, 7), base = 2)) %>%
  mutate(column = strtoi(str_sub(binary, 8, 10), base = 2)) %>%
  mutate(seat_id = row * 8 + column)
```

What is possible:
```R
my_data <- read_csv(input_filename, col_names = FALSE) %>%
  mutate(binary = str_replace_all(X1, "F|L", "0") %>% 
                  str_replace_all("B|R", "1"), # This is definitely nicer as there is no change of variable
        row = str_sub(binary, 1, 7) %>% strtoi (base = 2),
        column = str_sub(binary, 8, 10) %>% strtoi (base = 2),
        seat_id = row * 8 + column)
```

Rather than using expand.grid to find which two numbers in a set sum to a total, I could have used a neat shortcut:
`my_data %>% filter ((total - val) *in* val)`

Trees problem (day 3) - it didn't even enter my head to read the landscape into a data frame and work on it in the dataframe by generating sequences for the moves. This wouldn't have been elegant if the route ever involved going both up and down (as previous ouzzles have) but is worth bearing in mind rather than writing a while loop

If you ever use read.table and your data has # in it, be sure to define the comment character as "" (didn't happen to me as I used read_csv or readLines so far, but something to watch out for in the future)

Passport validation - it didn't enter my head to keep it at one row per passport per field, and check whether the individual fields were valid before pivoting wide (I pivoted wide early, filtered then joined back up with the original dataset)