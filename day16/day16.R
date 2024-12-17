library(tidyverse)

test  = read_lines("day16/test.txt") 
test2  = read_lines("day16/test2.txt") 
input = read_lines("day16/input.txt")

hash_details = function(h) {
  maphash(
    h, 
    function(k, v) { 
      cat("key: \n")
      print(str(k))
      cat("val: ", v, "\n")
      cat("\n")
    }
  )
}

dirs = list(
  "^" = c(-1,0),
  ">" = c(0,1),
  "v" = c(1,0),
  "<" = c(0,-1)
)

rots = list(
  "^" = c("<",">"),
  ">" = c("^","v"),
  "v" = c("<",">"),
  "<" = c("^","v")
)


m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

pts = which(m != "#" & m != "S", arr.ind = TRUE) 

dir = ">"
start = which(m == "S",arr.ind=TRUE)
end = which(m == "E",arr.ind=TRUE)

visit_score = hashtab(type = "identical", size = floor(sum(m==".")/3))
sethash(visit_score, list(pos = start, dir = dir), 0)

cand = list(
  list(pos = start, dir = dir, score = 0)
)

while (length(cand) > 0) {
  cur = cand[[1]]
  cand = cand[-1]
  
  prev_score = gethash(visit_score, list(pos = cur$pos, dir = cur$dir))
  
  if (!is.null(prev_score) && prev_score < cur$score)
    next
  
  new = cur$pos + dirs[[cur$dir]]
  if (m[new[1], new[2]] %in% c(".","E")) {
    prev_score = gethash(visit_score, list(pos = new, dir = cur$dir))
    
    if (is.null(prev_score) || prev_score > cur$score + 1 ) {
      sethash(visit_score, list(pos = new, dir = cur$dir), cur$score+1)
      cand[[length(cand)+1]] = list(pos = new, dir = cur$dir, score = cur$score+1)
    }
  }
  
  for (d in rots[[cur$dir]]) {
    prev_score = gethash(visit_score, list(pos = cur$pos, dir = d))
    
    if (is.null(prev_score) || prev_score > cur$score + 1000) {
      sethash(visit_score, list(pos = cur$pos, dir = d), cur$score+1000)
      cand[[length(cand)+1]] = list(pos = cur$pos, dir = d, score = cur$score+1000)
    }
  }
}

idx = 0
scores = numeric(4)
maphash(
  visit_score, 
  function(k, v) { 
    if (all(k$pos == end)) {
      idx <<- idx + 1
      scores[idx] <<- v
    }
  }
)

min(scores)


## Part 2

visited = list()

cands = list()
maphash(
  visit_score, 
  function(k, v) { 
    if (v ==  min(scores) ) {
      cands[[1]] <<- k
    }
  }
)

while (length(cands) > 0) {
  cur = cands[[1]]
  cands = cands[-1]
  
  cur_score = gethash(visit_score, list(pos = cur$pos, dir = cur$dir))
  
  visited[[length(visited) + 1]] = cur
  
  new = cur$pos - dirs[[cur$dir]]
  score = gethash(visit_score, list(pos = new, dir = cur$dir))
  if (!is.null(score) && score+1 == cur_score) {
    cands[[length(cands)+1]] = list(pos = new, dir = cur$dir)
  }
  
  for (d in rots[[cur$dir]]) {
    score = gethash(visit_score, list(pos = cur$pos, dir = d))
    if (!is.null(score) && score+1000 == cur_score) {
      cands[[length(cands)+1]] = list(pos = cur$pos, dir = d)
    }
  }
}

visited |>
  bind_rows() |>
  select(-dir) |>
  distinct() |>
  nrow() |>
  (\(x) x+1)()
