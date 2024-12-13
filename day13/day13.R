library(tidyverse)

test  = read_file("day13/test.txt") 
input = read_file("day13/input.txt")

input |>
  str_split("\n\n") |>
  unlist() |>
  str_match(
    "Button A: X\\+(\\d+), Y\\+(\\d+)\nButton B: X\\+(\\d+), Y\\+(\\d+)\nPrize: X=(\\d+), Y=(\\d+)"
  ) |>
  as.data.frame() |>
  set_names(c("full","ax","ay","bx","by","x","y")) |>
  select(-full) |>
  mutate(across(everything(), as.numeric)) |>
  as_tibble() |>
  pmap(
    function(ax,ay,bx,by,x,y) {
      expand_grid(a=1:100,b=1:100) |>
        mutate(
          calc_x = ax*a+bx*b,
          calc_y = ay*a+by*b,
          cost = 3*a+b
        ) |>
        filter(
          calc_x == x, calc_y == y
        ) |>
        slice_min(cost) |>
        pull(cost)
    }
  ) |>
  unlist() |>
  sum()

## Part 2

input |>
  str_split("\n\n") |>
  unlist() |>
  str_match(
    "Button A: X\\+(\\d+), Y\\+(\\d+)\nButton B: X\\+(\\d+), Y\\+(\\d+)\nPrize: X=(\\d+), Y=(\\d+)"
  ) |>
  as.data.frame() |>
  set_names(c("full","ax","ay","bx","by","x","y")) |>
  select(-full) |>
  mutate(across(everything(), as.numeric)) |>
  as_tibble() |>
  pmap(
    function(ax,ay,bx,by,x,y) {
        solve(
          matrix(
            c(ax,ay,bx,by), 2
          ),
          c(x+10000000000000,y+10000000000000)
        ) 
    }
  ) |>
  map(
    function(z) {
      z - round(z)
      if (all(abs(z - round(z)) < 1e-4))
        sum(z * c(3,1))
      else
        0
    }
  ) |>
  unlist() |>
  sum() |>
  formatC(digits = 16)

