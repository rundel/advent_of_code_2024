library(tidyverse)

test  = read_lines("day08/test.txt") 
input = read_lines("day08/input.txt")

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

freqs = unique(c(m)) |> setdiff(".")
freq_locs = map(freqs, ~ which(m == ., arr.ind = TRUE))

antinodes = list()

for (locs in freq_locs) {
  if (nrow(locs) == 1)
    next
  
  for(i in 1:(nrow(locs)-1)) {
    for (j in (i+1):nrow(locs)) {
      diff = locs[i,] - locs[j,]
      antinodes[[length(antinodes)+1]] = locs[i,] + diff
      antinodes[[length(antinodes)+1]] = locs[j,] - diff
    }
  }
}

antinodes |>
  bind_rows() |>
  filter(
    row >= 1 & row <= nrow(m),
    col >= 1 & col <= ncol(m)
  ) |>
  distinct() |>
  nrow()


## Part 2

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

freqs = unique(c(m)) |> setdiff(".")
freq_locs = map(freqs, ~ which(m == ., arr.ind = TRUE))

antinodes = list()

max_n = max(nrow(m), ncol(m))

for (locs in freq_locs) {
  if (nrow(locs) == 1)
    next
  
  for(i in 1:(nrow(locs)-1)) {
    for (j in (i+1):nrow(locs)) {
      diff = locs[i,] - locs[j,]
      for (k in seq_len(max_n)) {
        antinodes[[length(antinodes)+1]] = locs[i,] + diff * k
        antinodes[[length(antinodes)+1]] = locs[j,] - diff * k
      }
    }
  }
}

antinodes |>
  bind_rows() |>
  rbind(
    do.call(rbind, freq_locs)
  ) |>
  filter(
    row >= 1 & row <= nrow(m),
    col >= 1 & col <= ncol(m)
  ) |>
  distinct() |>
  nrow()
