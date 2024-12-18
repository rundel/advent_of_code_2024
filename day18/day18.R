library(tidyverse)

test  = read.csv("day18/test.txt", header = FALSE) 
input = read.csv("day18/input.txt", header = FALSE) 

d = test; n = 7; n_byte = 12
d = input; n = 71; n_byte = 1024

d = d |>
  transmute(
    r = V2 + 1,
    c = V1 + 1
  )

m = matrix(0, ncol=n, nrow=n)
for (i in seq_len(n_byte)) {
  m[d$r[i],d$c[i]] = 1
}

pts = which(m == 0, arr.ind = TRUE)
pts_lbl = apply(pts, 1, paste, collapse=",")

start = "1,1"
end = paste(c(n,n), collapse=",")

g = dist(pts) |>
  as.matrix() |>
  (\(x) which(x == 1, arr.ind = TRUE))() |>
  as.data.frame() |>
  transmute(
    from = pts_lbl[row],
    to = pts_lbl[col]
  ) |>
  as.matrix() |>
  igraph::graph_from_edgelist(directed = FALSE) 

g |>
  igraph::shortest_paths(start, end) |>
  (\(x) x$vpath[[1]] |> length())()


## Part 2

i = i + 1
repeat {
  print(i)
  m[d$r[i],d$c[i]] = 1
  
  pts = which(m == 0, arr.ind = TRUE)
  pts_lbl = apply(pts, 1, paste, collapse=",")
  
  g = dist(pts) |>
    as.matrix() |>
    (\(x) which(x == 1, arr.ind = TRUE))() |>
    as.data.frame() |>
    transmute(
      from = pts_lbl[row],
      to = pts_lbl[col]
    ) |>
    as.matrix() |>
    igraph::graph_from_edgelist(directed = FALSE) 
  
  l = g |>
    igraph::shortest_paths(start, end) |>
    (\(x) x$vpath[[1]] |> length())() 
  
  if (l == 0) {
    print(d[i,]-1 |> rev() |> unlist())
    break
  }
  
  i = i + 1
}