library(tidyverse)

test = read_lines("day04/test.txt") 
input = read_lines("day04/input.txt")

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

dirs = list(
  list(  0:3, rep(0,4)),
  list(0:-3, rep(0,4)),
  list(rep(0,4), 0:3),
  list(rep(0,4), 0:-3 ),
  list(0:3, 0:3),
  list(0:3, 0:-3),
  list(0:-3, 0:3),
  list(0:-3, 0:-3)
)

count = 0
for(i in seq_len(nrow(m))) {
  for(j in seq_len(ncol(m))) {

    for(dir in dirs) {
      x = i+dir[[1]]
      y = j+dir[[2]]
      #cat(dir[[1]],", ", dir[[2]],"\n")
      if (any(x < 1) | any(y < 1) | any(y > ncol(m)) | any(x > nrow(m))) {
        next
      }
      
      chars = map2_chr(x,y, ~m[.x,.y]) |>
        paste(collapse = "")
      if (chars == "XMAS") {
        count = count +1
        #cat(count,i,j,"\n")
        #cat(chars, "\n")
        #cat(x,":", y,"\n\n")
      }
    }
  }
}
count

## Part 2

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()


dirs = list(
  list(-1:1, -1:1),
  list(-1:1, 1:-1),
  list(1:-1, -1:1),
  list(1:-1, 1:-1)
)

count = 0
for(i in seq_len(nrow(m))) {
  for(j in seq_len(ncol(m))) {
    
    res = map_chr(
      dirs, 
      function(dir) {
        
    
      x = i+dir[[1]]
      y = j+dir[[2]]
      #cat(dir[[1]],", ", dir[[2]],"\n")
      if (any(x < 1) | any(y < 1) | any(y > ncol(m)) | any(x > nrow(m))) {
        return("")
      }
      
      map2_chr(x,y, ~m[.x,.y]) |>
        paste(collapse = "")
      }
    )
    
    if (sum(res == "MAS") == 2) {
      count = count +1
      
    }
  }
}
count
