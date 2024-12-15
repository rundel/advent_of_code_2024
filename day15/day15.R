library(tidyverse)

test  = read_file("day15/test.txt") 
test0 = read_file("day15/test0.txt") 
test1 = read_file("day15/test1.txt") 
input = read_file("day15/input.txt")

d = list(
  "^" = c(-1,0),
  ">" = c(0,1),
  "v" = c(1,0),
  "<" = c(0,-1)
)

x = input |>
  str_split("\n\n") |>
  unlist()

m = x[1] |>
  str_split("\n") |>
  unlist() |>
  str_split("") |>
  (\(x) do.call(rbind,x))()

steps = x[2] |>
  str_trim() |>
  str_remove_all("\n") |>
  str_split("") |>
  unlist()

apply(m,1,paste, collapse="") |>
  cat(sep="\n")

for(step in steps) {
  cat(step,"\n")
  pos = which(m == "@", arr.ind = TRUE)
  dir = d[[step]]
  
  end = pos
  repeat {
    end = end + dir
    if (m[end[1], end[2]] %in% c(".","#"))
      break
  }
  
  if (m[end[1],end[2]] == "#") {
    apply(m,1,paste, collapse="") |>
      cat(sep="\n")
    cat("\n")
    next
  }
  
  blocks = m[pos[1]:end[1], pos[2]:end[2]]
  blocks = c(".",blocks[-length(blocks)])
  
  m[pos[1]:end[1], pos[2]:end[2]] = blocks
  apply(m,1,paste, collapse="") |>
    cat(sep="\n")
  cat("\n")
}

which(m == "O",arr.ind = TRUE) |>
  as.data.frame() |>
  mutate(
    gps = (row-1)*100 + (col-1)
  ) |>
  summarize(
    sum(gps)
  )

## Part 2

board = function(m) {
  apply(m,1,paste, collapse="") |>
    cat(sep="\n")
}

x = input |>
  str_split("\n\n") |>
  unlist()

m = x[1] |>
  str_split("\n") |>
  unlist() |>
  str_replace_all("#", "##") |>
  str_replace_all("O", "[]") |>
  str_replace_all("\\.", "..") |>
  str_replace_all("@", "@.") |>
  str_split("") |>
  (\(x) do.call(rbind,x))() 


steps = x[2] |>
  str_trim() |>
  str_remove_all("\n") |>
  str_split("") |>
  unlist()


find_blocks = function(pos, step) {
  #browser()
  new = pos + d[[step]]
  if (m[new[1], new[2]] == c("[")) {
    return( c(
      list(new),
      find_blocks(new, step),
      find_blocks(new+c(0,1), step)
    ) )
  } else if (m[new[1], new[2]] == c("]")) {
    return( c(
      list(new+c(0,-1)),
      find_blocks(new, step),
      find_blocks(new+c(0,-1), step)
    ) )
  } else {
    return(NULL)
  }
}

can_move = function(b, step) {
  map_lgl(
    b,
    function(x) {
      new = x + d[[step]]
      !(m[new[1],new[2]] == "#" | m[new[1],new[2]+1] == "#")
    }
  )
}




pos = which(m == "@", arr.ind = TRUE)
step = "^"


for(step in steps) {
  if (any(capture.output(board(m)) |> str_detect("\\[[^\\]]|[^\\[]\\]"))) {
    stop()
  }
  cat(step,"\n")
  pos = which(m == "@", arr.ind = TRUE)
  dir = d[[step]]

  if (step %in% c("<",">")) {
    end = pos
    repeat {
      end = end + dir
      if (m[end[1], end[2]] %in% c(".","#"))
        break
    }
    
    if (m[end[1],end[2]] == "#") {
      apply(m,1,paste, collapse="") |>
        cat(sep="\n")
      cat("\n")
      next
    }
    
    blocks = m[pos[1]:end[1], pos[2]:end[2]]
    blocks = c(".",blocks[-length(blocks)])
    m[pos[1]:end[1], pos[2]:end[2]] = blocks
  } else {
    new = pos + d[[step]]
    if (m[new[1], new[2]] == "#") {
      
    } else if (m[new[1], new[2]] == ".") {
      m[new[1], new[2]] = "@"
      m[pos[1], pos[2]] = "."
    } else {
      blocks = find_blocks(pos, step) |> 
        unique() |>
        (\(x) do.call(rbind,x))() |>
        as.data.frame() 
      
      if (step == "^") {
        blocks = blocks|>
          arrange(desc(row)) |>
          pmap(function(row,col) c(row,col)) 
      } else {
        blocks = blocks|>
          arrange(row) |>
          pmap(function(row,col) c(row,col)) 
      }
      
      if (all(can_move(blocks, step))) {
        for (blk in blocks) {
          #new = blk + d[[step]]
          #m[new[1], new[2]+c(0,1)] = c("[","]")
          m[blk[1], blk[2]+c(0,1)] = c(".",".")
        }
        for (blk in blocks) {
          new = blk + d[[step]]
          m[new[1], new[2]+c(0,1)] = c("[","]")
          #m[blk[1], blk[2]+c(0,1)] = c(".",".")
        }
        m[pos[1],pos[2]] = "."
        pos = pos + d[[step]]
        m[pos[1],pos[2]] = "@"
      }
    }
  }
    
  board(m)
  cat("\n")
}

which(m == "[",arr.ind = TRUE) |>
  as.data.frame() |>
  mutate(
    gps = (row-1)*100 + (col-1)
  ) |>
  summarize(
    sum(gps)
  )
