library(tidyverse)

test  = read_file("day14/test.txt") 
input = read_file("day14/input.txt")

nx = 101; ny = 103
#nx = 11; ny = 7

d = input |>
  str_trim() |>
  str_split("\n") |>
  unlist() |>
  str_match("p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)") |>
  as.data.frame() |>
  set_names(c("pat","px","py","vx","vy")) |>
  select(-pat) |>
  mutate(
    across(everything(), as.double)
  ) |>
  as_tibble()


d = d |>
  mutate(
    px = abs( (px + vx * 100) %% nx ),
    py = abs( (py + vy * 100) %% ny )
  )

m = matrix(0, nrow=ny, ncol=nx)
for(i in 1:nrow(d)) {
  m[d$py[i]+1, d$px[i]+1] = m[d$py[i]+1, d$px[i]+1] + 1
}

mx = ceiling(nx/2)
my = ceiling(ny/2)
c(
  m[1:(my-1), 1:(mx-1)] |> sum(),
  m[(my+1):ny, 1:(mx-1)] |> sum(),
  m[1:(my-1), (mx+1):nx] |> sum(),
  m[(my+1):ny, (mx+1):nx] |> sum()
) |>
  prod()


## Part 2

plot_field = function(d,main = "") {
  m = matrix(0, nrow=ny, ncol=nx)
  for(i in 1:nrow(d)) {
    m[d$py[i]+1, d$px[i]+1] = m[d$py[i]+1, d$px[i]+1] + 1
  }
  image(m, main = as.character(main))
}

d = input |>
  str_trim() |>
  str_split("\n") |>
  unlist() |>
  str_match("p=(\\d+),(\\d+) v=(-?\\d+),(-?\\d+)") |>
  as.data.frame() |>
  set_names(c("pat","px","py","vx","vy")) |>
  select(-pat) |>
  mutate(
    across(everything(), as.double)
  ) |>
  as_tibble()



d = d |>
  mutate(
    px = abs( (px + vx * 89) %% nx ),
    py = abs( (py + vy * 89) %% ny )
  )

plot_field(d,89)



j = 89

for (k in 1:100) {N
  j = j + 103
  d = d |>
    mutate(
      px = abs( (px + vx * 103) %% nx ),
      py = abs( (py + vy * 103) %% ny )
    )
  
  plot_field(d,j)
}
