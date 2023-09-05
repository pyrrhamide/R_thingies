
pacman::p_load(tibble, data.table)

set.seed(14)

t <- tibble(
  "AAAHA" = runif(5), 
  "foaf.saawxs" = rnorm(5), 
  "camelCase" = letters[1:5], 
  "wh.at.ev.er" = rep("whatevs", 5))

t |> janitor::clean_names()

setDT(t)

janitor::clean_names(t) # ça marche aussi, tip top 
