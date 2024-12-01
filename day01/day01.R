library(tidyverse)

test = read_delim("day01/test.txt", delim = "   ", col_names = c("x","y")) 
input = read_delim("day01/input.txt", delim = "   ", col_names = c("x","y")) 

input |>
  mutate(
    x = sort(x),
    y = sort(y),
    diff = abs(x-y)
  ) |>
  summarize(
    sum(diff)
  )


# Task 2

x = unique(input$x)
times = table(input$y)[as.character(x)] 

found = names(times)
sub = !is.na(found)

sum(x[sub] * times[sub])
  