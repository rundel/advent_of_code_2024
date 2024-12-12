library(tidyverse)

test  = read_lines("day12/test.txt") 
test2  = read_lines("day12/test2.txt") 
test3  = read_lines("day12/test3.txt") 
test4  = read_lines("day12/test4.txt") 
input = read_lines("day12/input.txt")

flatten = function(x) {
  if (inherits(x, "POLYGON")) {
    list(x)
  } else {
    map(x, ~st_polygon(.x))
  }
}

m = input |>
  str_split("") |> 
  (\(x) do.call(rbind,x))()

m2 = factor(m) |> as.integer()
dim(m2) = dim(m)

g = starsExtra::matrix_to_stars(m2) |> 
  sf::st_as_sf() |>
  group_by(A1) |>
  summarise(n = n())

g |>
  st_geometry() |> 
  map(flatten) |>
  unlist(recursive = FALSE) |>
  st_sfc() |>
  (\(g) sf::st_area(g) * sf::st_perimeter(g))() |>
  sum()
 

## Part 2

sides = function(x) {
  map_int(
    x, 
    ~ map_int(.x, function(p) {
      nrow(p)-1
    }) |> 
      sum()  
  )
}

x = g |>
  st_simplify() |>
  st_geometry() |> 
  map(flatten) |>
  unlist(recursive = FALSE) |>
  st_sfc()

x |>
  (\(g) sf::st_area(g) * sides(g))() |>
  sum()

