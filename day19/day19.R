library(tidyverse)

test  = read_file("day19/test.txt") 
input = read_file("day19/input.txt") 



## Part 1

x = str_split(input, "\n\n") |>
  unlist()

pats = str_split(x[1],", ") |>
  unlist()

towels = x[2] |>
  str_trim() |>
  str_split("\n") |>
  unlist()

pats = pats[order(nchar(pats), decreasing = TRUE)]

pat = paste0("^(", paste(pats, collapse="|"), ")")

cache = list()

towel_possible = function(towel) {
  val = cache[[towel]]
  if (!is.null(val))
    return(val)
  
  if (nchar(towel) == 0) {
    cache[[towel]] <<- TRUE
    return(TRUE)
  }
  poss = str_match(towel, paste0("^",pats)) |>
    na.omit() |>
    c()
  
  if (length(poss) == 0) {
    cache[[towel]] <<- FALSE
    return(FALSE)
  }
  
  for(p in poss) {
    res = str_remove(towel, paste0("^",p)) |>
      towel_possible()
    
    if (res) {
      cache[[towel]] <<- TRUE
      return(TRUE)
    }
  }
  
  cache[[towel]] <<- FALSE
  return(FALSE)
}



(res = map_lgl(towels, function(x) {cat(x,"\n");towel_possible(x)}, .progress = TRUE))
sum(res)


## Part 2

x = str_split(input, "\n\n") |>
  unlist()

pats = str_split(x[1],", ") |>
  unlist()

towels = x[2] |>
  str_trim() |>
  str_split("\n") |>
  unlist()

pats = pats[order(nchar(pats), decreasing = TRUE)]

pat = paste0("^(", paste(pats, collapse="|"), ")")

cache = list()

towel_count = function(towel) {
  #print(towel)
  val = cache[[towel]]
  if (!is.null(val))
    return(val)
  
  if (nchar(towel) == 0) {
    return(1)
  }
  poss = str_match(towel, paste0("^",pats)) |>
    na.omit() |>
    c()
  
  if (length(poss) == 0) {
    cache[[towel]] <<- 0
    return(0)
  }
  
  n = 0
  for(p in poss) {
    res = str_remove(towel, paste0("^",p)) |>
      towel_count()
    
    n = n + res 
  }
  
  cache[[towel]] <<- n
  return(n)
}

(res = map_dbl(towels, function(x) {cat(x,"\n");towel_count(x)}, .progress = TRUE))
sum(res) |>
  formatC(digits=16)
