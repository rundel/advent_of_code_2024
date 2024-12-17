library(tidyverse)
library(bit64)
source("https://raw.githubusercontent.com/GjjvdBurg/R-bitops64/refs/heads/master/bitops64.R")

test  = read_file("day17/test.txt") 
test2 = read_file("day17/test2.txt")
input = read_file("day17/input.txt")

combos = function(x) {
  case_when(
    x == 0 ~ as.integer64(0),
    x == 1 ~ as.integer64(1),
    x == 2 ~ as.integer64(2),
    x == 3 ~ as.integer64(3),
    x == 4 ~ as.integer64(A),
    x == 5 ~ as.integer64(B),
    x == 6 ~ as.integer64(C)
  )
}


opcodes = function(op1, op2) {
  if (op1 == 0) {
    A <<- as.integer64(A / 2^combos(op2))
    inst <<- inst + 2
  } else if (op1 == 1) {
    #B <<- bitwXor(B, op2)
    B <<- bXor(B, op2)
    inst <<- inst + 2
  } else if (op1 == 2) {
    B <<- combos(op2) %% 8
    inst <<- inst + 2
  } else if (op1 == 3) {
    if (A != 0) {
      inst <<- as.numeric(op2+1)
    } else {
      inst <<- inst + 2
    }
  } else if (op1 == 4) {
    B <<- bXor(B, C)
    inst <<- inst + 2
  } else if (op1 == 5) {
    out <<- c(out, combos(op2) %% 8)
    inst <<- inst + 2
  } else if (op1 == 6) {
    B <<- as.integer64(A / 2^combos(op2))
    inst <<- inst + 2
  } else if (op1 == 7) {
    C <<- as.integer64(A / 2^combos(op2))
    inst <<- inst + 2
  }
}


x = input |>
  str_match("Register A: (\\d+)
Register B: (\\d+)
Register C: (\\d+)

Program: ([0-9,]+)
") |>
  (\(x) c(x[-1]))() |>
  (\(x) list(
    A=x[1] |> as.integer64(),
    B=x[2] |> as.integer64(),
    C=x[3] |> as.integer64(), 
    prog = x[4] |> str_split(",") |> unlist() |> as.integer64(),
    inst = 1,
    out = integer64()
  ))()

## Part 1

attach(x, warn.conflicts = FALSE)

while (inst < length(prog)) {
  if (inst == 0)
    browser()
  opcodes(prog[inst], prog[inst+1])
}
out

## Part 2

facts = as.integer64(8)^(15:0)

for (i in 1:16) {
  if (i == 1) {
    d = data.frame(0:7)
  } else {
    d = d |>
      expand_grid(0:7, .name_repair = "minimal")
  }
  
  keep = rep(FALSE, nrow(d))
  for (j in seq_len(nrow(d))) {
    vals = c(as.matrix(d)[j,], rep(0, 16-i))
    
    attach(x, warn.conflicts = FALSE)
    A = sum(facts * vals)
    
    while (inst < length(prog)) {
      opcodes(prog[inst], prog[inst+1])
    }
    
    if (length(out) == length(prog)) {
      keep[j] = all(out[(16-(i-1)):16] == prog[(16-(i-1)):16])
    }
  }
  d = d[keep,]
  cat("i =",i,"\n")
}

res = integer64()
for (j in seq_len(nrow(d))) {
  vals = c(as.matrix(d)[j,])
  res = c(res, sum(facts * vals))
}

