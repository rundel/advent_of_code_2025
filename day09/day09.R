library(tidyverse)

test = read.csv("day09/test.txt", header = FALSE)
input = read.csv("day09/input.txt", header = FALSE)

## Task 1

input |>
  cross_join(input) |>
  set_names(c("x1","y1","x2","y2")) |>
  mutate(
    area = (abs(x1-x2)+1)*(abs(y1-y2) +1)
  ) |>
  slice_max(area)

## Task 2

library(sf)

d = input

p = rbind(d, d[1,,drop=FALSE]) |>
  as.matrix() |>
  list() |>
  st_polygon() 

rects = d |>
  cross_join(d) |>
  set_names(c("x1","y1","x2","y2")) |>
  filter(
    x1 != x2 # 
  ) |>
  filter(
    y1 != y2
  ) |>
  pmap(
    function(x1,y1,x2,y2) {
      st_polygon(
        list(
          matrix(
            c(
              x1,y1,
              x2,y1,
              x2,y2,
              x1,y2,
              x1,y1
            ),
            ncol = 2,
            byrow = TRUE
          )
        )
      )
    }
  )

i = st_contains(p, st_sfc(rects)) 

z = map_dbl(
  rects[unlist(i)], 
  function(x) {
    st_buffer(x, 0.5, nQuadSegs = 0) |>
    st_area()
  }
) + 0.5

max(z)