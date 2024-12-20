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
  
  d = dist(pts) |>
    as.matrix() |>
    (\(x) which(x <= 20 & x > 0, arr.ind=TRUE))() |>
    as.data.frame() |>
    transmute(
      from = pts_lbl[row],
      to = pts_lbl[col]
    ) 
  
  
  d2 = d |>
    mutate(
      forward = match(to, path) - match(from, path)
    ) |>
    filter(
      forward > 0
    ) |>
    select(-forward) 
  
  to_mat = function(x) {
    str_split(x,",") |> 
      unlist() |> 
      as.numeric() |> 
      matrix(byrow = TRUE,ncol=2)
  }
  
  d2 |>
    mutate(
      sc_dist = (to_mat(from) - to_mat(to)) |> apply(1, function(x) sum(abs(x)))
    ) |>
    filter(sc_dist <= 20) |>
    mutate(
      i = match(from, rownames(gd)),
      j = match(to, rownames(gd)),
      k = (j-1)*nrow(gd) + i,
      sc_saved = gd[k],
      saved = sc_saved - sc_dist
    ) |>
    arrange(desc(saved)) |>
    as_tibble() |>
    filter(saved >= cutoff)
}

(res = task2(test,50))

res |> pull(saved) |> table()


(x=task2(input,100))
nrow(x)
