# advent2020

# Day 10: things I learned when working on the puzzle
* diff() # vector of differences between consecutive numbers in a vector
* rle() # run length equivalent - represents a sequence as each value, then the number of times that value appears consecutively, like how images used to be encoded in the 1980s (pixel colour, number of pixels of that colour)

# Days 8 and 9: things I learned from others' solutions
break() exists, so no need to set a variable to TRUE and do a while loop until I set it to FALSE inside the loop;

# Day 7: things I learned from others' solutions
Get the edges out of igraph for a particular path:
```R
# syntax
E(graph, path = path)$weight

# for example
paths_from_my_bag <- all_simple_paths(my_graph, my_bag, mode = "out")
sapply(seq_along(paths_from_my_bag), function(x){
  prod(E(my_graph, path = paths_from_my_bag[[x]])$weight)
  })
```


# Day 6: things I learned while working on the puzzle
Grouping and summarising
* count(x,y) is equivalent to group(x,y) %>% tally() %>% ungroup - IT UNGROUPS AT THE END< YAY!
* add_count(x,y) adds a column to your existing table, meaning that you don't have to left_join back up at the end

# Day 6: things I learned from others' solutions

Parsing data - examples
* extract a length plus units in one line:
```R
extract(v, c("height", "unit"), "(\\d+)(cm|in)", convert = TRUE, remove = FALSE)
```
* extract key:value pairs separated by spaces
```R
mutate(m = str_match_all(x, "(...)\\:([^ ]+)")) %>% 
  mutate(f = map(m, ~ .[, 2]),
         v = map(m, ~ .[, 3]))
```


Grouping and summarising
* As distinct can take multiple arguments, we don't have to drop other columns before using it, e.g.
```R
%>% distinct(b,c)
```
instead of
```R
%>% select (-a) %>% distinct()
```

If you want unique IDs within groups, then dplyr::row_number() respects grouping - but does also rank the answers

Split string into *vector* rather than into list by grabbing the first element of the list up front:
```R
a <- str_split("abc", "")[[1]]
```

intersect may be helpful in the future:
```R
intersect(c("a", "b", "c"), c("b", "c", "d"))
[1] "b" "c"
```


# Days 1-5: things I learned from others' solutions
(I have not updated my own original solutions to reflect these, but may use them going forwards)

readr::parse_number - extracts number from string - I didn't know about this at all


tidyr::separate_rows can be useful a lot eaerlier than I had realised in the process e.g, 
`separate_rows(my_data, sep = "\n\n")`
This means that I didn't need to parse out the double newlines from one of the input files (which also contained single newlines within the data)
[Used on day 6]


Allocating unique IDs to rows (e.g. before separating them further)
* tibble::rowid_to_column - does what it says on the tin (much nicer than what I did: initial_data$my_id <- 1:nrow(initial_data))
[Used on day 6 - but see below!]
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