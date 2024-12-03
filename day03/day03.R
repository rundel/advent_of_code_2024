library(tidyverse)

test = readr::read_lines("day03/test.txt")
input = readr::read_lines("day03/input.txt")

x = str_match_all(test, "mul\\((\\d{1,3}),(\\d{1,3})\\)")[[1]]
sum(as.numeric(x[,2]) * as.numeric(x[,3]))


x = str_match_all(input, "mul\\((\\d{1,3}),(\\d{1,3})\\)")
x = do.call(rbind, x)
sum(as.numeric(x[,2]) * as.numeric(x[,3]))

## Task 2

test2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

x = str_match_all(test2, "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)")[[1]]

state = TRUE
x = str_match_all(input, "mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\)")
x = do.call(rbind, x)

sum = 0
for(i in seq_len(nrow(x))) {
  if (x[i,1] == "don't()") 
    state = FALSE
  else if (x[i,1] == "do()")
    state = TRUE
  else if (state) {
    sum = sum + as.numeric(x[i,2]) * as.numeric(x[i,3])
  }
}

sum


