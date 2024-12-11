library(tidyverse)

test  = read_lines("day11/test.txt") 
input = read_lines("day11/input.txt")

x = input |>
  str_split(" ") |>
  unlist() |>
  as.integer()

n_digits = function(x) {
  floor(log(x,10))+1
}

step = function(x) {
  d = n_digits(x)
  
  if (x == 0) {
    1
  } else if (d %% 2 == 0) {
    l = floor(x / 10^(d/2))
    r = x - l * 10^(d/2)
    c(l,r)
  } else {
    x * 2024
  }
}

for (i in 1:25) {
  x = map(x, step) |>
    unlist()
}
length(x)


## Part 2

x = input |>
  str_split(" ") |>
  unlist() |>
  as.integer()

max_depth=75
cache = list()

count = function(x, depth=0) {
  xc = as.character(x)
  if (depth == max_depth) {
    return(1)
  } 
  
  if (is.null(cache[[xc]])) {
    cache[[xc]] <<- rep(NA, max_depth+1)
  }
  
  if (is.na(cache[[xc]][depth+1])) {
    d = n_digits(x)
    if (x == 0) {
      n = count(1, depth+1)
    } else if (d %% 2 == 0) {
      l = floor(x / 10^(d/2))
      r = x - l * 10^(d/2)
      n = count(l, depth+1) +
          count(r, depth+1)
    } else {
      n = count(x * 2024, depth+1)
    }
    
    cache[[xc]][depth+1] <<- n
  } 
  
  return( cache[[xc]][depth+1] )
}
  
z = map_dbl(x, count)
z |> 
  sum() |> 
  formatC(digits = 16)
