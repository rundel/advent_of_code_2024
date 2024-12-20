library(tidyverse)

test  = read_lines("day20/test.txt") 
input = read_lines("day20/input.txt") 

make_graph = function(m) {
  pts = which(m != "#", arr.ind=TRUE)
  pts_lbl = apply(pts, 1, paste, collapse=",")
  
  dist(pts) |>
    as.matrix() |>
    (\(x) which(x == 1, arr.ind=TRUE))() |>
    as.data.frame() |>
    transmute(
      from = pts_lbl[row],
      to = pts_lbl[col]
    ) |>
    as.matrix() |>
    igraph::graph_from_edgelist(directed=FALSE) |>
    igraph::simplify()
}

task1 = function(input) {
  m = input |>
    str_split("") |>
    (\(x) do.call(rbind, x))()
  

  
  start = which(m == "S", arr.ind = TRUE) |> paste(collapse=",")
  end = which(m == "E", arr.ind = TRUE) |> paste(collapse=",")
  
  base = igraph::shortest_paths(make_graph(m), from = start, to = end, output = "epath")$epath |>
    map_int(length) |>
    min()
  
  pts = which(m != "#", arr.ind=TRUE)
  pts_lbl = apply(pts, 1, paste, collapse=",")
  
  poss_sc = dist(pts) |>
    as.matrix() |>
    (\(x) which(x == 2, arr.ind=TRUE))() |>
    as.data.frame() |>
    transmute(
      from = pts_lbl[row],
      to = pts_lbl[col]
    )
  
  gd = igraph::distances(make_graph(m))
  pmap_dbl(
    poss_sc,
    function(from, to) {
      gd[from, to]
    }
  ) |>
    (\(x) (x-2) >= 100 )() |>
    sum() |>
    (\(x) x/2)() # Double counting
}

to_vec = function(x) {
  x |>
    str_split(",") |>
    unlist() |>
    as.numeric()
}

#task1(input)

## Part 2

task2 = function(input, cutoff=100) {
  m = input |>
    str_split("") |>
    (\(x) do.call(rbind, x))()
  
  start = which(m == "S", arr.ind = TRUE) |> paste(collapse=",")
  end = which(m == "E", arr.ind = TRUE) |> paste(collapse=",")
  
  path = igraph::shortest_paths(make_graph(m), from = start, to = end, output = "vpath")$vpath[[1]] |>
    names()
  
  gd = igraph::distances(make_graph(m))
  
  pts = which(m != "#", arr.ind=TRUE)
  pts_lbl = apply(pts, 1, paste, collapse=",")
  
  dist(pts) |>
    as.matrix() |>
    (\(x) which(x <= 20 & x > 0, arr.ind=TRUE))() |>
    as.data.frame() |>
    transmute(
      from = pts_lbl[row],
      to = pts_lbl[col]
    ) |>
    mutate(
      forward = map_int(to, ~which(path == .x)) - map_int(from, ~which(path == .x))
    ) |>
    filter(
      forward > 0
    ) |>
    select(-forward) |>
    mutate(
      sc_dist = map2_dbl(from, to, ~sum(abs(to_vec(.x) - to_vec(.y)))),
      sc_saved = map2_dbl(from, to, ~gd[.x, .y]),
      saved = sc_saved - sc_dist
    ) |>
    arrange(desc(saved)) |>
    as_tibble() |>
    filter(saved >= cutoff) |>
    pull(saved) |>
    table()
}

task2(test,50)
