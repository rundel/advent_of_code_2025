library(tidyverse)

test = read_lines("day01/test.txt")
input = read_lines("day01/input.txt")

d = input |> 
  str_match("([LR])(\\d+)") |>
  as_tibble() |>
  set_names(c("command", "dir", "steps")) |>
  mutate(
    steps = as.integer(steps),
    steps = ifelse(dir == "L", -steps, steps)
  )

## Task 1

c(50, d$steps) |> cumsum() |> (function(x) x %% 100)() |> (function(x) sum(x == 0))()

## Task 2 

x = c(50, d$steps) |> cumsum()

trim = function(x) {
  x[c(-1, -length(x))]
}

map2(lag(x)[-1], x[-1], ~ trim(.x:.y) %% 100) |>
  map_int(~ sum(.x == 0)) |>
  sum() +
  c(50, d$steps) |> cumsum() |> (function(x) x %% 100)() |> (function(x) sum(x == 0))()


