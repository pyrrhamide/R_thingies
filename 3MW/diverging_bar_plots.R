
# How to create diverging bar plots

# Albert Rapp (comme d'hab)

# lien : https://albert-rapp.de/posts/ggplot2-tips/22_diverging_bar_plot/22_diverging_bar_plot
# replicating a plot from PEW Research Center 

# packages ----
library(tidyverse)

# créer les données ----
original_dat <- tribble(
  ~label, ~group, ~strongly_oppose, ~somewhat_oppose, ~somewhat_favor, ~strongly_favor, ~neither, ~no_experience,
  'Total', 'total', 22, 18, 15, 9, 32, 4,
  'Men', 'gender', 25, 19, 13, 8, 32, 3,
  'Women', 'gender', 20, 17, 18, 9, 32, 3,
  'Ages 18-29', 'age', 16, 17, 19, 12, 32, 4,
  '30-49', 'age', 20, 16, 17, 10, 33, 3,
  '50-64', 'age', 25, 18, 13, 8, 32, 4,
  '65+', 'age', 27, 20, 13, 5, 31, 4,
  'High school or less', 'education', 23, 16, 13, 9, 32, 7,
  'Some college', 'education', 21, 18, 15, 9, 35, 2,
  'Bachelor\'s degree', 'education', 20, 20, 20, 8, 31, 1,
  'Postgraduate', 'education', 23, 19, 20, 8, 30, 1,
  'Lower Income', 'income', 21, 16, 15, 9, 32, 7,
  'Middle income', 'income', 22, 18, 16, 9, 33, 2,
  'Upper income', 'income', 22, 21, 18, 9, 30, 0
  
)
original_dat

# on arrange les données pour avoir un bon format ggplot()
dat_longer <- original_dat |> 
  pivot_longer(-c(label, group),
               names_to = "preference",
               values_to = "percentage")

# 'and for the diverging bar chart part, we don't actually need the "neither" and "no experience" group
dat_diverging <- dat_longer |> 
  filter(!preference %in% c("neither", "no_experience"))

# on a les données, maintenant il faut créer les repères pour le graphique ----
