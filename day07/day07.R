library(tidyverse)

test = read_lines("day07/test.txt")
input = read_lines("day07/input.txt")

## Task 1

m = input |>
  str_split("") |>
  (\(x) do.call(rbind,x))()

p = which(m == "S", arr.ind = TRUE)
m[p[1], p[2]] = "|"

n = 0
for(r in seq_len(nrow(m)-1)) {
  for(c in which(m[r,] == "|")) {
    if (m[r+1, c] == ".") {
      m[r+1, c] = "|"
    } else if (m[r+1, c] == "^") {
      m[r+1, c-1] = "|"
      m[r+1, c+1] = "|"
      n = n+1
    }
  }
}
n

## Task 2

pts = which(m != ".", arr.ind = TRUE) |>
  as.data.frame() |>
  mutate(
    symbol = m[m!="."],
    label = paste0(row,",",col)
  ) |>
  arrange(row, col) |>
  mutate(
    i = seq_len(n())
  )

adj = matrix(0, nrow=nrow(pts), ncol=nrow(pts))
colnames(adj) = pts$label

pwalk(
  pts,
  function(row, col, symbol, label, i) {
    if (symbol == "|") {
      j = which(pts$label == paste0(row+1,",",col))
      if (length(j)!= 0)
        adj[i, j] <<- 1
    } else if (symbol == "^") {
      j1 = which(pts$label == paste0(row+1,",",col-1))
      j2 = which(pts$label == paste0(row+1,",",col+1))

      adj[i, j1] <<- 1
      adj[i, j2] <<- 1
    }
  }
)

state = t(c(1,rep(0,nrow(pts)-1)))

for (i in seq_len(max(pts$row)-1)) {
  state = state %*% adj
}

sum(state) |>
  format(scientific = FALSE)


