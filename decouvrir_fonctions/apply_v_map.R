library(tidyverse)

# générer la liste
l <- list(
  c(1:5), c(3:7), c(6:10)
)

l

# fonction compilée
l[[1]] |> mean()
l[[2]] |> mean()
l[[3]] |> mean()

l |> map(mean)
l |> map_dbl(mean)

lapply(l, mean)
sapply(l, mean)


l |> map(is.numeric)
l |> map_lgl(is.numeric)

lapply(l, is.numeric)
sapply(l, is.numeric)

# fonction anonyme
l |> map(~ . * 2)

lapply(l, \(x) x*2)   # \(x) ou function(x)
