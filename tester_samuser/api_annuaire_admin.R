
# API annuaire de l'administration et des services publics
# https://www.data.gouv.fr/fr/dataservices/api-annuaire-de-ladministration-et-des-services-publics/

# pour observer et déterminer comment construire la requête 
# https://api-lannuaire.service-public.fr/explore/dataset/api-lannuaire-administration/api/

library(httr2)
library(tidyverse)
library(data.table)

base_url <- "https://api-lannuaire.service-public.fr/api/explore/v2.1" |> request()

api_annuaire_minju <- base_url |> 
  req_url_path_append("catalog/datasets/api-lannuaire-administration/records") |> 
  req_url_query(select = c("partenaire", "nom", "sigle", "partenaire_identifiant", "id", "categorie", "type_organisme", "code_insee_commune"), .multi = "comma") |> 
  req_url_query(where = "partenaire LIKE 'ministere_justice' and nom LIKE 'centre pénitentiaire'",
                limit = 100,
                offset = 0) |> 
  req_perform()

api_annuaire_minju$status_code == 200

api_annuaire_minju |> 
  resp_body_json() |> glimpse()

deb_cp <- api_annuaire_minju |> 
  resp_body_json() |>
  pluck("results") |> 
  enframe() |> 
  unnest_wider(value)

# -----

wtf_url <- base_url |> 
  req_url_path_append("catalog/datasets/api-lannuaire-administration/exports/json")

wtf_rep <- wtf_url |> req_perform()
wtf_rep |> resp_body_json() |> glimpse()

# aaaah d'accords, j'ai tout l'annuaire (catalog/records donne qu'un échantillon limité pour explo, catalog/exports permet de télécharger le jdd complet). c'est après json que je peux appliquer mes filtres 


# -----

minju_req <- base_url |> 
  req_url_path_append("catalog/datasets/api-lannuaire-administration/exports/json") |> 
  req_url_query(select = c("partenaire", "nom", "sigle", "partenaire_identifiant", "id", "categorie", "type_organisme", "code_insee_commune", "ancien_code_pivot", "pivot"), .multi = "comma") |> 
  req_url_query(where = "partenaire LIKE 'ministere_justice'") |> 
  req_perform()

minju_organismes <- minju_req |> 
  resp_body_json() |> 
  enframe() |> 
  unnest_wider(value)

casser_pivot <- minju_organismes |>
  filter(!is.na(pivot)) |>
  select(id, pivot) |>
  mutate(pivot = map(pivot, ~ jsonlite::fromJSON(., flatten = T))) |>
  unnest_wider(pivot) |> 
  unnest_longer(code_insee_commune) |> 
  rename(codes_communes = code_insee_commune)

minju_organismes <- minju_organismes |> 
  left_join(casser_pivot, by = "id")

minju_organismes |> count(type_service_local) |> print(n=Inf)
# type "spip" est pas le SPIP que je connais :')
# c'est le service de réparation pénale ici...

# établissements pénitentiaires parce que je suis une tarée

etab_penit <- minju_organismes |> 
  filter(nom %like% "^(Centre pénitentiaire|Maison d'arrêt|Maison centrale|Centre de détention|Centre de semi-liberté|Etablissement pénitentiaire)") |> 
  arrange(nom)

## ou type_service_local %in% c("centre_detention", "centre_penitentiaire", "csl", "esm", "maison_arret", "maison_centrale")

nrow(etab_penit)

view(etab_penit)
# il va y avoir un nouveau CD dans le Vaucluse ! CD du Comtat Venaissin
# + MA MONACO ???

etab_penit |> filter(is.na(ancien_code_pivot)) |> select(nom)

# tribunaux

minju_organismes |> 
  filter(nom %like% "^Tribunal judiciaire") |> 
  arrange(nom) |> 
  view()

# SAS ???

minju_organismes |> 
  filter(nom %like% "Structure d'accompagnement") # nyet
