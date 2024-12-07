library(tidyverse)

test  = read_lines("day07/test.txt") 
input = read_lines("day07/input.txt")

z = test |>
  str_split(": ") |>
  (\(x) do.call(rbind, x))() |>
  as.data.frame() |>
  set_names(c("test","vals")) |>
  mutate(
    test = as.double(test),
    vals = str_split(vals, " ") |>
      map(as.double)
  ) |>
  pmap_dbl(
    function(test, vals) {
      n = length(vals)-1
      d = do.call(
        expand.grid,
        rep(list(c("*","+")), n)
      )
      exprs = vals[1]
      for (i in seq_len(n)) {
        exprs = paste0("(",exprs, d[[i]], vals[i+1],")")
      }
      
      if (any(test == map_dbl(exprs, ~ eval(parse(text = .x)))))
        test
      else
        0
    }
  )

z |> sum()

## Part 2

future::plan(future::multisession(workers=8))

z = input |>
  str_split(": ") |>
  (\(x) do.call(rbind, x))() |>
  as.data.frame() |>
  set_names(c("test","vals")) |>
  mutate(
    test = as.double(test),
    vals = str_split(vals, " ") |>
      map(as.double)
  ) |>
  #(\(x) x[501:600,])() |>
  furrr::future_pmap_dbl(
    function(test, vals) {
      n = length(vals)-1
      d = do.call(
        expand.grid,
        rep(list(c("*","+", "%|||%")), n)
      )
      exprs = vals[1]
      for (i in seq_len(n)) {
        exprs = paste0("(",exprs, d[[i]], vals[i+1],")")
      }
      
      `%|||%` <- function(x,y) {
        n = nchar(as.character(y))
        x * 10^n + y
      }
      
      res = map_dbl(exprs, ~ eval(parse(text = .x)))
      
      if (any(test == res))
        test
      else
        0
    },
    .progress = TRUE
  )

z |> sum() |> formatC(digits=16)

