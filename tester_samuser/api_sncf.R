
library(httr2)
library(tidyverse)

base_api <- request('https://ressources.data.sncf.com/api/explore/v2.1')

datasets_available <- base_api |> 
  req_url_path_append("catalog") |> 
  req_url_path_append("exports") |> 
  # req_url_query(select = c("dataset_id", "fields"), .multi = "comma") |> 
  # req_url_query(limit = 100) |> 
  req_perform()

datasets_available |> 
  resp_body_json() |> #simplifyVector fait un gros boulot à lui seul
  pluck(2) |> 
  map_dfr(\(x) tibble(
    dataset_id = pluck(x, "dataset_id"),
    dataset_uid = pluck(x, "dataset_uid"),
    has_records = pluck(x, "has_records")
    )
  )

req_gares <- base_api |> 
  req_url_path_append("catalog") |> 
  req_url_path_append("datasets") |> 
  req_url_path_append("liste-des-gares") |> 
  req_url_path_append("exports") |> 
  req_url_path_append("json") |> 
  req_perform()

gares_sncf <- req_gares |> resp_body_json(simplifyVector = TRUE)

gares_sncf |> count(fret, voyageurs)

gares_sncf |> 
  filter(voyageurs == "O") |> 
  as_tibble() |> view()


req_frequentations <- base_api |> 
  req_url_path_append("catalog") |> 
  req_url_path_append("datasets") |> 
  req_url_path_append("frequentation-gares") |> 
  req_url_path_append("exports") |> 
  req_url_path_append("json") |> 
  req_perform()

frequentations <- req_frequentations |> 
  resp_body_json(simplifyVector = T) |> 
  as_tibble() |>
  janitor::clean_names() |> 
  rename(total_voyageurs_2017 = totalvoyageurs2017)

map_chr(frequentations, class)

freq_gares <- frequentations |> 
  select(!contains("non_voyageurs")) |> 
  mutate(code_postal = str_pad(code_postal, 5, side = "left", pad = "0")) |> 
  # filter(code_postal == "94350") |> 
  pivot_longer(
    starts_with("total_voyageurs")
    , names_prefix = "total_voyageurs_"
    , names_to = "annee"
    , values_to = "voyageurs"
    ) 

freq_gares |> 
  filter(str_starts(code_postal, "94")) |> 
  arrange(nom_gare, annee) |> 
  mutate(idc_freq = (voyageurs-first(voyageurs))/first(voyageurs)*100,
         .by = nom_gare) |> 
  ggplot(aes(x = annee, y = idc_freq, group = nom_gare, colour = nom_gare)) +
  geom_line()
