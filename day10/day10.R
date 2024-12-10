library(tidyverse)

test  = read_lines("day10/test.txt") 
input = read_lines("day10/input.txt")

m = input |>
  str_split("") |>
  map(as.integer) |>
  (\(x) do.call(rbind, x))()

m[is.na(m)] = -1

dirs = list(
  c(-1,0),
  c(1,0),
  c(0,1),
  c(0,-1)
)

get_val = function(r,c) {
  if (r < 1 | r > nrow(m) | c < 1 | c > ncol(m)) {
    -1
  } else {
    m[r,c]
  }
}

count_trails = function(r,c, path = c()) {
  #print(c(r,c))
  
  x = m[r,c]
  
  if (x == 9) {
    return(paste0("(",r,",", c,")"))
  }
  
  new_pos = map(dirs, ~c(r,c) + .x)
  new_val = map_int(new_pos, ~get_val(.x[1],.x[2]))
  
  idx = which(new_val == x+1) 
  
  if (length(idx) == 0) {
    return(NULL)
  } else {
    map(
      idx,
      function(i) {
        path = c(path, paste0("(",new_pos[[i]][1],",", new_pos[[i]][2],")"))
        count_trails(new_pos[[i]][1], new_pos[[i]][2], path)
      }
    ) |>
      unlist()
  }
}

which(m == 0, arr.ind = TRUE) |>
  apply(1, function(x) {
    count_trails(x[1],x[2]) |> unique() |> length()
  }) |>
  sum()
  

## Part 2

m = input |>
  str_split("") |>
  map(as.integer) |>
  (\(x) do.call(rbind, x))()

m[is.na(m)] = -1

dirs = list(
  c(-1,0),
  c(1,0),
  c(0,1),
  c(0,-1)
)

get_val = function(r,c) {
  if (r < 1 | r > nrow(m) | c < 1 | c > ncol(m)) {
    -1
  } else {
    m[r,c]
  }
}

count_trails = function(r,c, path = c()) {
  #print(c(r,c))
  
  x = m[r,c]
  
  if (x == 9) {
    return(1)
  }
  
  new_pos = map(dirs, ~c(r,c) + .x)
  new_val = map_int(new_pos, ~get_val(.x[1],.x[2]))
  
  idx = which(new_val == x+1) 
  
  if (length(idx) == 0) {
    return(0)
  } else {
    map_int(
      idx,
      function(i) {
        path = c(path, paste0("(",new_pos[[i]][1],",", new_pos[[i]][2],")"))
        count_trails(new_pos[[i]][1], new_pos[[i]][2], path)
      }
    ) |>
      sum()
  }
}

which(m == 0, arr.ind = TRUE) |>
  apply(1, function(x) {
    count_trails(x[1],x[2]) 
  }) |>
  sum()



