library(tidyverse)

test = read_lines("day03/test.txt")
input = read_lines("day03/input.txt")

## Task 1

m = input |>
  str_split("") |>
  map(as.numeric)

i = map_dbl(m, ~ which.max(.x[-length(.x)]))
j = map2_dbl(m, i, ~ which.max(.x[-(1:.y)])+.y)

map_dbl(seq_along(m), ~ m[[.x]][i[.x]]*10 + m[[.x]][j[.x]]) |>
  sum()


## Task 2

drop_final_n = function(x, n) {
  x[1:(length(x)-n+1)]
}

m = input |>
  str_split("") |>
  map(as.numeric)

n = length(m[[1]])
digits = 12

indexes = list()
values = rep(0, length(m))
start_index = rep(0, length(m))

for(i in seq_len(digits)) {
  indexes[[i]] = rep(NA, length(m))
  for(j in seq_along(m)) {
    x = m[[j]]
    if (start_index[j] != 0) {
      x = x[-seq_len(start_index[j])]
    }
    k = which.max(drop_final_n(x, digits-i+1)) + start_index[j]
    indexes[[i]][j] = k
    start_index[j] = k
    
    values[j] = values[j]*10+m[[j]][ k ]
  }
}

sum(values) |>
  format(scientific = FALSE)


