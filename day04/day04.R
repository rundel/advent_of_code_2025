library(tidyverse)

test = read_lines("day04/test.txt")
input = read_lines("day04/input.txt")

## Task 1

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()

d = which(m == "@", arr.ind = TRUE) |>
  dist() |>
  as.matrix()

sum( apply(d, 1, function(x) sum(x < 1.5)) <= 4 )

## Task 2

m = input |>
  str_split("") |>
  (\(x) do.call(rbind, x))()
prev = m

n_start = sum(m=="@")

repeat {
  locs = which(m == "@", arr.ind = TRUE)
  d = locs |>
    dist() |>
    as.matrix()
  
  sub = apply(d, 1, function(x) sum(x < 1.5)) <= 4
  prev = m
  
  sublocs = locs[sub,,drop=FALSE]
  for (i in seq_len(nrow(sublocs))) {
    m[sublocs[i,1], sublocs[i,2]] = "."
  }

  print(sum(m=="@"))
  
  if (all(m == prev))
    break
}

n_end = sum(m=="@")

n_start - n_end


