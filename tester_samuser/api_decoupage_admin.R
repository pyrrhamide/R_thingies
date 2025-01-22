
# Albert Rapp, you will always be famous 
# https://youtu.be/hmtE4QGIOuk?si=6vqsAg-K3tQYRtr-

# ich : API découpage administratif de la DINUM
# https://www.data.gouv.fr/fr/dataservices/api-decoupage-administratif-api-geo/

library(tidyverse)
library(httr2)

# ------------ code commune : un cas donné ------------

# 1) entrer l'url de l'api, avec le code déjà renseigné
geo_api_url <- 'https://geo.api.gouv.fr/communes?code=44109'

# 2) créer une requête ? 
geo_api_reponse <- request(geo_api_url) |> 
  # appliquer la requête et récupérer les résultats 
  req_perform()

# 3) observer le résultat de la requête => du JSON 
geo_api_reponse |> resp_body_json() # moche
# format liste. pour observer la structure :
geo_api_reponse |> resp_body_json() |> glimpse() # disons que je vise nom et codeEpci

# ooooh ptit argument de la fonction resp_body_json() pour applatir direct
geo_api_reponse |> resp_body_json(simplifyVector = TRUE)

# bref, si je veux prendre un élément en particulier
geo_api_reponse |> 
  resp_body_json() |> 
  pluck(1, "nom") 

# et si je veux en prendre plus d'un ?
geo_api_reponse |> 
  resp_body_json() |>
  map_dfr(
    \(x) tibble(
      lb_com = pluck(x, "nom"),
      code_epci = pluck(x, "codeEpci")
    )
  )


# ------------- toutes les communes d'un département ---------------

geo_api_url2 <- 'https://geo.api.gouv.fr/departements/94/communes'

geo_api_response2 <- request(geo_api_url2) |> req_perform()

geo_api_response2 |> resp_body_json() # oh
geo_api_response2 |> 
  resp_body_json() |> 
  map_dfr(
    \(x) tibble(
      commune = x |> pluck("nom"),
      code_com = x |> pluck("code"),
      code_dep = x |> pluck("codeDepartement"),
      code_epci = x |> pluck("codeEpci"),
      pop = x |> pluck("population")
    ) # omg je vais crier sur TOUS LES TOITS 
  ) |> view()

# VSM 17ème ville la plus peuplée du 94 (sur 47)

# ------------- prep fonction de recherche de la commune --------------

# oki, objectif maintenant est de rendre ça ~interactif~ en input le code (dep ou commune, whatevs) que je veux
# on va faire simple. je veux la commune

code_commune <- '72181'
geo_api_url_com <- glue::glue('https://geo.api.gouv.fr/communes/{code_commune}')

reponse_geo_api <- request(geo_api_url_com) |> req_perform()

reponse_geo_api |> 
  resp_body_json(simplifyVector = TRUE) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  distinct(nom, code_com = code, population)

# en fonction 

recherche_code_com <- function(code_commune){
  geo_api_url_com <- glue::glue('https://geo.api.gouv.fr/communes/{code_commune}')
  
  reponse_geo_api <- request(geo_api_url_com) |> req_perform()
  
  reponse_geo_api |> 
    resp_body_json(simplifyVector = TRUE) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    distinct(nom, code_com = code, population)
}

recherche_code_com("01053")
recherche_code_com("94080")
recherche_code_com("94059") # :) Plessis, et non VSM :)
recherche_code_com("01001")
recherche_code_com("75056")
recherche_code_com("75120") # il y a bien les arrondissements 
recherche_code_com("97401")


# url code postal = 'https://geo.api.gouv.fr/communes?codePostal={code}'
# faudrait conditionner sur si on entre un code commune ou un code postal 
code_postal <- '94350'
geo_api_url_com <- glue::glue('https://geo.api.gouv.fr/communes?codePostal={code_postal}')

reponse_geo_api <- request(geo_api_url_com) |> req_perform()

reponse_geo_api |> 
  resp_body_json(simplifyVector = TRUE) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  distinct(nom, code_com_insee = code, population)

recherche_commune <- function(code_commune, code_postal = FALSE) {
  
  if (code_postal) {
    cli::cli_alert_info("Recherche par code postal")
    geo_api_url_com <- glue::glue('https://geo.api.gouv.fr/communes?codePostal={code_commune}')
  } else {
    cli::cli_alert_info("Recherche par code commune Insee")
    geo_api_url_com <- glue::glue('https://geo.api.gouv.fr/communes/{code_commune}')
  }
  
  reponse_geo_api <- request(geo_api_url_com) |> req_perform()
  
  reponse_geo_api |> 
    resp_body_json(simplifyVector = TRUE) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    distinct(nom, code_com_insee = code, population)
}

recherche_commune("94350", code_postal = T)
recherche_commune("94079")

recherche_commune("01280", code_postal = T)
recherche_commune("01313")

recherche_commune("97400", code_postal = T)


# plus propre
recherche_commune <- function(code_commune, code_postal = FALSE, nom_commune = FALSE) {
  
  geo_api <- "https://geo.api.gouv.fr/communes" |> 
    request() 
  
  if (code_postal) {
      geo_api_rep <- geo_api |> req_url_query(codePostal = code_commune)
    } else if (nom_commune) {
      geo_api_rep <- geo_api |> 
        req_url_query(
          nom = code_commune,
          boost = "population",
          limit = 6
          )
    } else {
      geo_api_rep <- geo_api |> req_url_path_append(code_commune) 
    }
  
  geo_api_rep <- geo_api_rep |> 
    req_url_query(
      fields = c("nom", "code AS code_com_insee", "population"), 
      .multi = "comma"
    ) |> 
    req_perform()
    
  geo_api_rep |> 
    resp_body_json(simplifyVector = TRUE) |> 
    as_tibble()
}

recherche_commune("72181")
recherche_commune("Villiers", nom_commune = T)
