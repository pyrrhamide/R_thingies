library(tidyverse)

irve_raw <- read_csv("https://www.data.gouv.fr/fr/datasets/r/eb76d20a-8501-400e-b336-d85724de5435")

glimpse(irve_raw)

irve_raw |> summarise(sum(nbre_pdc))

irve_raw |> 
  select(starts_with("id_")) |> 
  summarise(across(everything(), data.table::uniqueN)) |> 
  pivot_longer(everything())

zbeul <- irve_raw |> 
  filter(date_maj == max(date_maj),.by = id_pdc_local)

zbeul |> summarise(sum(nbre_pdc))

zbeul2 <- zbeul |> 
  mutate(nb_id_station_local = n(), .by = id_station_local) |> 
  mutate(pb = nb_id_station_local == nbre_pdc) 
