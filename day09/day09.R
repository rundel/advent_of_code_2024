library(tidyverse)

test  = read_file("day09/test.txt") |> str_trim()
input = read_file("day09/input.txt") |> str_trim()

x = input |>
  str_split("") |>
  unlist() |>
  as.integer()

if (length(x) %% 2 == 1)
  x = c(x, 0)


i = seq(1, length(x), 2)
ids = seq_along(i) - 1
s = map(ids, ~ c(.x, ".")) |>
  unlist() |>
  rep(x)

repeat {
  cur_empty = min(which(s == "."))
  cur_data = max(which(s %in% ids))

  if (cur_empty > cur_data)
    break

  s[cur_empty] = s[cur_data]
  s[cur_data] = "."
}

s[s != "."] |>
  as.integer() |>
  (\(x) x * (seq_along(x) - 1))() |>
  sum() |>
  formatC(digits = 16)

## Part 2

x = input |>
  str_split("") |>
  unlist() |>
  as.integer()

if (length(x) %% 2 == 1)
  x = c(x,0)

i = seq(1,length(x), 2)
ids = map(seq_along(i)-1, ~ c(.x, -1)) |>
  unlist()
size = x
moved = rep(c(FALSE,TRUE), length(x)/2)


p = progress_estimated(sum(!moved))

while (!all(moved)) {
  n = max(which(!moved))
  #browser
  if (ids[n] == -1) {
    moved[n] = TRUE
    #moved = moved[-n]
    #ids = ids[-n]
    #size = size[-n]
    next
  }
  
  i = which(ids == -1 & size[n] <= size)
  if (length(i) == 0 | all(i > n)) {
    moved[n] = TRUE
    next
  }
  i = min(i)
  
  if (size[n] == size[i]) {
    ids[i] = ids[n]
    ids[n] = -1
    moved[i] = TRUE 
  } else {
    
    ids = c(
      ids[1:(i-1)],
      ids[n],
      -1,
      ids[(i+1):(n-1)],
      -1,
      ids[-(1:n)]
    )
    
    size = c(
      size[1:(i-1)],
      size[n],
      size[i] - size[n],
      size[(i+1):(n-1)],
      size[n],
      size[-(1:n)]
    )
    
    moved = c(
      moved[1:(i-1)],
      TRUE,
      TRUE,
      moved[(i+1):(n-1)],
      TRUE,
      moved[-(1:n)]
    )
  }
 
  p$tick()$print()
}

rep(ids, size) |>
  (\(x) {x[x==-1] = 0;x})() |>
  (\(x) x * (seq_along(x) - 1))() |>
  sum() |>
  formatC(digits = 16)
