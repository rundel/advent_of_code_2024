library(tidyverse)

test = read_file("day05/test.txt") 
input = read_file("day05/input.txt")

x = str_split(input, "\n\n")[[1]]
rules = x[1] |> 
  str_split("\n") |> 
  unlist() |>
  str_split("\\|") |>
  map(as.integer) |>
  (\(x) do.call(rbind, x))()

books = x[2] |>
  str_trim() |>
  str_split("\n") |> 
  unlist() |>
  str_split(",") |>
  map(as.integer)

check_book = function(book, rules) {
  for(i in seq_along(book)) {
    banned = rules[,1][rules[,2] == book[i]]
    if (any(banned %in% book[-(1:i)]))
      return(0)
  }
  
  return(book[ceiling(length(book)/2)])
}

map_int(books, check_book, rules=rules) |>
  sum()


## Part 2

x = str_split(input, "\n\n")[[1]]
rules = x[1] |> 
  str_split("\n") |> 
  unlist() |>
  str_split("\\|") |>
  map(as.integer) |>
  (\(x) do.call(rbind, x))()

books = x[2] |>
  str_trim() |>
  str_split("\n") |> 
  unlist() |>
  str_split(",") |>
  map(as.integer)

invalid = books[map_int(books, check_book, rules=rules) == 0]

fix_book = function(book, rules) {
  repeat {
    valid = TRUE
    for(i in seq_along(book)) {
      banned = rules[,1][rules[,2] == book[i]]
      j = which(book[-(1:i)] %in% banned) + i
      
      if (length(j) != 0) {
        j = max(j)
        if (j != length(book))
          book = c(book[1:j][-i], book[i], book[(j+1):length(book)])
        else
          book = c(book[1:j][-i], book[i])
        #print(book)
        break
      }
    }
    
    x = check_book(book, rules)
    if (x != 0 )
      return(x)
  }
}

map_int(invalid, fix_book, rules=rules) |>
  sum()
