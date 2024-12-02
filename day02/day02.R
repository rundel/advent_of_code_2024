library(tidyverse)

test = read_lines("day02/test.txt")
input = read_lines("day02/input.txt")

safe = function(x) {
  (all(x > 0) | all(x < 0)) & all(abs(x) %in% 1:3)
}

# Part 1

input |>
  str_split(" ") |>
  map(as.integer) |>
  map(diff) |>
  map_lgl(safe) |>
  sum()


# Part 2

input |>
  str_split(" ") |>
  map(as.integer) |>
  map_lgl(
    function(x) {
      map_lgl(
        seq_along(x),
        function(i) {
          x[-i] |>
            diff() |>
            safe()
        }
      ) |>
        any()
    }
  ) |>
  sum()