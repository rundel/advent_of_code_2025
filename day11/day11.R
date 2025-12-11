library(tidyverse)

test = read_lines("day11/test.txt")
test2 = read_lines("day11/test2.txt")
input = read_lines("day11/input.txt")

## Task 1

d = input |>
  str_match("([a-z]+):((?: [a-z]+)+)") |>
  as.data.frame() |>
  set_names(c("x","from","to")) |>
  select(-x) |>
  mutate(
    to = to |>
      str_trim() |>
      str_split(" ")
  ) |>
  unnest_longer(to)

g = igraph::graph_from_edgelist(as.matrix(d), directed = TRUE)
igraph::all_simple_paths(g, from = "you", to = "out") |>
  length()

## Task 2

d = input |>
  str_match("([a-z]+):((?: [a-z]+)+)") |>
  as.data.frame() |>
  set_names(c("x","from","to")) |>
  select(-x) |>
  mutate(
    to = to |>
      str_trim() |>
      str_split(" ")
  ) |>
  unnest_longer(to)

g = igraph::graph_from_edgelist(as.matrix(d), directed = TRUE)

m = igraph::as_adjacency_matrix(g)

igraph::shortest_paths(g, from = "svr", to = "fft")
igraph::shortest_paths(g, from = "fft", to = "dac")
igraph::shortest_paths(g, from = "dac", to = "out")

(srv_fft = igraph::all_simple_paths(g, from = "svr", to = "fft", cutoff = 10)) |> length()
(fft_dac = igraph::all_simple_paths(g, from = "fft", to = "dac", cutoff = 18)) |> length()
(dac_out = igraph::all_simple_paths(g, from = "dac", to = "out", cutoff = 10)) |> length()

((length(srv_fft)*1) * (length(fft_dac)*1) * (length(dac_out)*1)) |>
  format(scientific = FALSE)
