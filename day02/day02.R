library(tidyverse)

test = read_lines("day02/test.txt")
input = read_lines("day02/input.txt")

## Task 1

is_valid = function(x) {
  n = nchar(x)
  if (n %% 2 == 1)
    return(TRUE)
  
  x1 = substr(x, 1, floor(n/2))
  x2 = substr(x, floor(n/2)+1, n)
  x1 != x2
}

x = input |>
  str_split(",") |>
  unlist() |>
  str_split("-")


z = x |>
  map(
    function(x) {
      r = x[1]:x[2]
      r[!map_lgl(r, is_valid)]
    }
  ) |>
  unlist()

z |>  
  as.numeric() |>
  sum()

## Task 2

library(mirai)

is_valid = function(x) {
  n = nchar(x)
  if (n == 1)
    return(TRUE)
  
  d = unlist(str_split(x, ""))
  for (i in 1:floor(n/2)) {
    if (n %% i != 0)
      next
    
    pat = rep(d[1:i], n/i)
    if (all(pat == d))
      return(FALSE)
  }
  
  TRUE
}

x = input |>
  str_split(",") |>
  unlist() |>
  str_split("-")

daemons(10)
z = x |>
  map(
    in_parallel(
      function(x) {
        r = x[1]:x[2]
        r[!map_lgl(r, is_valid)]
      },
      is_valid = is_valid,
      map_lgl = purrr::map_lgl,
      str_split = stringr::str_split
    ),
    .progress = TRUE
  ) |>
  unlist()

z |>  
  as.numeric() |>
  sum()

 