library(tidyverse)

test = read.csv("day08/test.txt", header = FALSE); n=10
input = read.csv("day08/input.txt", header = FALSE); n=1000

## Task 1

d = input |>
  dist() |>
  as.matrix()

nm = input |>
  apply(1, paste, collapse=",")

smallest = sort(d[d!=0] |> unique())[1:1000]

adj = (d <= smallest[length(smallest)] & d > 0) * 1
colnames(adj) = nm

g = igraph::graph_from_adjacency_matrix(adj)
plot(g)
igraph::components(g)$csize |>
  sort(decreasing = TRUE) |>
  (\(x) x[1:3])() |>
  prod()


# Task 2

df = input

d = df |>
  dist() |>
  as.matrix()

smallest = sort(d[d!=0] |> unique())

n = 4900
repeat {
    adj = (d <= smallest[n] & d > 0) * 1

    g = igraph::graph_from_adjacency_matrix(adj)

    if (igraph::count_components(g) == 1)
        break

    if (n %% 100 == 0)
        cat(n,"\n")
  
    n = n+1
}

idx = which(d == smallest[n], arr.ind = TRUE)[1,]
as.numeric(df$V1[idx[1]]) * as.numeric(df$V1[idx[2]])

