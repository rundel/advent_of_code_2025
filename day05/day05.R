library(tidyverse)

test = read_file("day05/test.txt")
input = read_file("day05/input.txt")

## Task 1

x = input |>
  str_split("\n\n") |>
  unlist()

r = x[1] |>
  str_split("\n") |>
  unlist() |>
  str_split("-") |>
  map(as.numeric)

g = x[2] |>
  str_trim() |>
  str_split("\n") |>
  unlist() |>
  as.numeric()

map_lgl(g, function(x) any(map_lgl(r, ~ x >= .x[1] & x <= .x[2]))) |>
  sum()


## Task 2

library(sf)

x = input |>
  str_split("\n\n") |>
  unlist()

r = x[1] |>
  str_split("\n") |>
  unlist() |>
  str_split("-") |>
  map(as.numeric) |>
  map(~cbind(.x,0)) |>
  map(~st_linestring(.x)) |>
  st_sfc()

st_buffer(r, 0.1) |> 
  st_union() |> 
  st_geometry() |> 
  st_cast("POLYGON") |>
  map(st_bbox) |>
  map(round) |>
  map_dbl(~ .x[3] - .x[1] + 1) |>
  sum() |>
  format(scientific = FALSE)
 

