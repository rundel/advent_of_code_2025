library(tidyverse)

test = read_file("day12/test.txt")
input = read_file("day12/input.txt")

## Task 1

rotate = function(m) {
  r = list(1:3, 1:3, 3:1, 3:1)
  c = list(1:3, 3:1, 1:3, 3:1)

  c(
    map2(r,c, ~m[.x, .y]),
    map2(r,c, ~t(m)[.x, .y])
  ) |> unique()
}

d = input

trees = d |>
  str_match_all("(\\d+)x(\\d+):((?: \\d+)+)") |>
  as.data.frame() |>
  set_names(c("x", "c", "r", "i")) |>
  select(-x) |>
  mutate(
    r = as.integer(r),
    c = as.integer(c),
    i = i |> str_trim() |> str_split(" ") |> map(as.integer)
  ) |>
  as_tibble()

presents = d |>
  str_match_all("\\d+:\n((?:[#.]{3}\n){3})") |>
  (\(x) x[[1]][,2])() |>
  str_split("\n") |>
  map( ~ do.call(rbind, str_split(.x, ""))) 

presents_rot = presents |>
  map(rotate)

n_mark = map_int(presents, ~ sum(.x == "#"))
n_needed = map_int(trees$i, ~sum(.x*n_mark))
keep = trees$r * trees$c >= n_needed

sum(keep)



# trees

# join_patch = function(m1,m2) {
#   ifelse(m1 == "#" | m2 == "#", "#", ".")
# }

# hash = function(m, need) {
#   h1 = paste(m, collapse="")
#   h2 = paste(need, collapse=",")
#   paste(h1,h2)
# }

# res = list()

# place = function(m, need) {
#   nr = nrow(m); nc = ncol(m)
#   if (sum(need*n_mark) > nr*nc)
#     return(FALSE)

#   if (!is.null(res[[hash(m,need)]])) {
#     return(res[[hash(m,need)]])
#   }
  
#   if (all(need == 0)) {
#     return(TRUE)
#   }

#   for(i in which(need != 0)) {
#     for(pres in presents_rot[[i]]) {
#       for(r in seq_len(nr-2)) {
#         for(c in seq_len(nc-2)) { 
#           m_sub = m[r+0:2, c+0:2]
#           if (!any(pres == m_sub & m_sub != ".")) {
#             m_new = m
#             m_new[r+0:2, c+0:2] = join_patch(pres, m_sub)
#             need_new = need
#             need_new[i] = need_new[i]-1
#             #print(need_new)
#             #print(m_new)
#             if (place(m_new, need_new)) {
#               res[[hash(m,need)]] <<- TRUE
#               return(TRUE)
#             }
#           }
#         }
#       }
#     }
#   }
  

#   res[[hash(m,need)]] <<- FALSE
#   FALSE
# }

# i = 3
# m = matrix('.', trees$r[i], trees$c[i])
# need = trees$i[[i]]

# place(m, need)
