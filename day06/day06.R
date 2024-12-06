library(tidyverse)

test = read_lines("day06/test.txt") 
input = read_lines("day06/input.txt")

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

dirs = list(
  c(-1,0),
  c(0,1),
  c(1,0),
  c(0,-1)
)

cur_pos = start = which(m == "^", arr.ind = TRUE)
cur_dir = 0

m[cur_pos[1], cur_pos[2]] = "X"

repeat {
  #visits[[length(visits)+1]] = cur_pos
  next_pos = cur_pos + dirs[[cur_dir+1]]
  
  if (next_pos[1] < 1 | next_pos[1] > nrow(m) | next_pos[2] < 1 | next_pos[2] > ncol(m))
    break
  
  if (m[next_pos[1], next_pos[2]] == "#") {
    cur_dir = (cur_dir+1) %% 4
    next
  }
  
  m[next_pos[1], next_pos[2]] = "X"
  cur_pos = next_pos
}

sum(m == "X")



## Part 2

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

cur_pos = start = which(m == "^", arr.ind = TRUE)
cur_dir = 0

route = list()

repeat {
  #visits[[length(visits)+1]] = cur_pos
  next_pos = cur_pos + dirs[[cur_dir+1]]
  route[[length(route)+1]] = cur_pos
  
  if (next_pos[1] < 1 | next_pos[1] > nrow(m) | next_pos[2] < 1 | next_pos[2] > ncol(m)) {
    break
  }
    
  
  if (m[next_pos[1], next_pos[2]] == "#") {
    cur_dir = (cur_dir+1) %% 4
    next
  }
  
  
  cur_pos = next_pos
}

# Obstacles can only occur on the previous path
r = do.call(rbind, route) |>
  as.data.frame() |>
  distinct()

loops = 0
max_iter = 10000 # Run long enough to assume a loop
for(i in seq_len(nrow(r))[-1]) { # skip start
  m2 = m
  m2[r$row[i],r$col[i]] = "#"
  
  cur_pos = start
  cur_dir = 0
  
  iter = 0
  repeat {
    #visits[[length(visits)+1]] = cur_pos
    next_pos = cur_pos + dirs[[cur_dir+1]]
    #route[[length(route)+1]] = cur_pos
    
    if (next_pos[1] < 1 | next_pos[1] > nrow(m) | next_pos[2] < 1 | next_pos[2] > ncol(m)) {
      break
    }
    
    if (m2[next_pos[1], next_pos[2]] == "#") {
      cur_dir = (cur_dir+1) %% 4
      next
    }
    
    
    cur_pos = next_pos
    iter = iter + 1
    if(iter > max_iter) {
      loops = loops + 1
      #print(r[i,])
      break
    }
  }
}

loops



# Much Better

future::plan(future::multisession(workers=8))

res = furrr::future_map_lgl(
  seq_len(nrow(r))[-1],
  function(i) {
    m2 = array("", dim=c(nrow(m),ncol(m), 4))
    
    tmp = m
    tmp[r$row[i],r$col[i]] = "#"
    
    m2[,,1] = tmp
    m2[,,2] = tmp
    m2[,,3] = tmp
    m2[,,4] = tmp
    
    cur_pos = start
    cur_dir = 0
    
    iter = 0
    repeat {
      #visits[[length(visits)+1]] = cur_pos
      next_pos = cur_pos + dirs[[cur_dir+1]]
      m2[cur_pos[1], cur_pos[2], cur_dir+1] = "X"
      
      if (next_pos[1] < 1 | next_pos[1] > nrow(m) | next_pos[2] < 1 | next_pos[2] > ncol(m)) {
        return(FALSE)
      }
      
      if (m2[next_pos[1], next_pos[2], 1] == "#") {
        cur_dir = (cur_dir+1) %% 4
        next
      }
      
      if (m2[next_pos[1], next_pos[2], cur_dir+1] == "X") {
        return(TRUE)
      }
      
      cur_pos = next_pos
    }
  },
  .progress = TRUE
) 
  
res |> sum()

