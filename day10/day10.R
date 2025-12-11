library(tidyverse)

test = read_lines("day10/test.txt")
input = read_lines("day10/input.txt")

## Task 1

df = input |>
  str_split("\\]|\\{") |>
  (\(x) do.call(rbind, x))() |>
  as.data.frame() |>
  set_names(c("lights", "buttons", "joltage")) |>
  as_tibble() |>
  mutate(
    lights = str_remove(lights, "\\[") |>
      str_split(""),
    buttons = str_remove_all(buttons, "\\(|\\)") |>
      str_trim() |>
      str_split(" ") |>
      map(str_split, ",") |>
      map(~ map(.x, as.numeric)),
    joltage = str_remove(joltage, "\\}") |>
      str_split(",") |>
      map(as.integer)
  )

light_to_int = function(l) {
  (2^(seq_along(l) - 1) * ifelse(l == "#", 1, 0)) |>
    sum()
}

button_to_int = function(b) {
  sum(2^(b))
}

df2 = df |>
  select(-joltage) |>
  mutate(
    lights = map_int(lights, light_to_int),
    buttons = map(buttons, ~ map_int(.x, button_to_int))
  )

find_n_press = in_parallel( 
  function(l,b) {
    n_max = 100000
    n_samp = 30
    for(i in seq_len(n_max)) {
      pushes = sample(b, n_samp, replace = TRUE)
      res = accumulate(pushes, bitops::bitXor)
      i = which(res == l)
      if (length(i) == 0) {
        n_samp = n_samp
      } else {
        n_samp = min(i)
      }
    }
    n_samp
  },
  accumulate = purrr::accumulate  
)

library(mirai)
daemons(10)

(res = map2_int(
  df2$lights, df2$buttons, 
  find_n_press,
  .progress = TRUE
))

sum(res)

## Task 2

button_to_vec = function(b,n) {
  v = rep(0,n)
  v[b+1] = 1
  v
}

df = input |>
  str_split("\\]|\\{") |>
  (\(x) do.call(rbind, x))() |>
  as.data.frame() |>
  set_names(c("lights", "buttons", "joltage")) |>
  as_tibble() |>
  mutate(
    lights = str_remove(lights, "\\[") |>
      str_split(""),
    buttons = str_remove_all(buttons, "\\(|\\)") |>
      str_trim() |>
      str_split(" ") |>
      map(str_split, ",") |>
      map(~ map(.x, as.numeric)),
    joltage = str_remove(joltage, "\\}") |>
      str_split(",") |>
      map(as.integer)
  )

df3 = df |>
  select(-lights) |>
  mutate(
    buttons = map2(buttons, map_int(joltage, length), ~ map(.x, button_to_vec, .y))
  )

b = df3$buttons[[1]]
j = df3$joltage[[1]]

(res = map2_dbl(
  df3$buttons, df3$joltage,
  function(b,j) {
    lpSolve::lp(
      direction = "min",
      objective.in = rep(1,length(b)),
      const.mat = do.call(cbind, b),
      const.dir = "=",
      const.rhs = j,
      all.int = TRUE
    )$solution |>
      sum()
  },
  .progress = TRUE
))

sum(res)
