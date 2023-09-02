library(tidyverse)

na <- starwars |> filter(if_any(everything(),~is.na(.x)))

df <- tibble(
  item_name = c("a","a","b","b"),
  group = c(1,NA,1,2),
  value1 = c(1,NA,3,4),
  value2 = c(4,5,NA,7)
)

df |> filter(if_any(everything(),~is.na(.x)))

starwars |> select(where(is.numeric)) |> rowwise() |> mutate(sum = sum(c_across(everything())))
