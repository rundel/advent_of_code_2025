library(tidyverse)

test = read_lines("day06/test.txt")
input = read_lines("day06/input.txt")

## Task 1

input |>
  str_squish() |>
  str_split(" ") |>
  (\(x) do.call(rbind,x))() |>
  as.data.frame() |>
  map(
    function(x) {
      reduce(as.numeric(x[-length(x)]), paste0('`',x[length(x)],'`') |> parse(text = _) |> eval())
    }
  ) |>
  unlist() |>
  sum() |>
  format(scientific = FALSE)


## Task 2

m = input |>
  str_split("") |>
  (\(x) do.call(rbind,x))() 

splits = apply(m, 2, function(x) all(x == " ")) |>
  which()

vals = map2(
  c(1,splits+1),
  c(splits-1, ncol(m)),
  function(x,y) m[,x:y]
)

map_dbl(
  vals,
  function(x) {
    op = x[nrow(x),] |>
      paste(collapse="") |>
      str_trim() |>
      (\(z) paste0('`', z ,'`'))() |> 
      parse(text = _) |> 
      eval()

    x[-nrow(x),] |>
      apply(2, paste, collapse="") |>
      str_trim() |>
      as.numeric() |>
      reduce(op)
  }
) |>
  sum() |>
  format(scientific=FALSE)

