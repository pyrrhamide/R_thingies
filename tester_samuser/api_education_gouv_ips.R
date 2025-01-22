
# Indices de position sociale ------------------------------

## Introduction -------------

# Données du ministère de l'éducation nationale
# Ecole primaire, collège, lycée

# IPS permet d'appréhender le statut social des élèves à partir des PCS des parents (questionnaire de début d'année). A chaque PCS ou couple de PCS est associée une valeur numérique de l'IPS. Cette valeur numérique correspond à un résumé quanti d'un ensemble d'attributs socio-éco et culturels liés à la réussite scolaire. **Plus l'IPS est élevé, plus les élèves sont en moyenne d'origine sociale favorisée. Plus il est faible, plus les élèves sont d'origine défavorisée socialement.**

# Première version de l'indice, utilisée jusqu'à la rentrée 2021, calculée sur les données du panel de la DEPP d'élèves entrés en sixième en 2007 (donc ont eu le bac en 2014). 
# Rentrée 2022 onwards : données du panel de la DEPP d'élèves entrés en 2011 en CP (bac en 2023).

# Le niveau social d'un établissement scolaire = calcul de la moyenne des IPS des élèves qui y sont scolarisés. 
# Résumé simplifié de la réalité qui ne peut rendre compte à lui seul de la complexité de la situation socio-eco et culturel des établissements accueillis dans un étab.

# **Indice d'hétérogénéité sociale d'un établissement** = écart type de l'IPS de ses élèves. Plus il est élevé, plus le profil social des élèves est diversifié (source de hausse du niveau scolaire il me semble, cf Dorian). Indice calculé depuis rentrée 2019 pour établissements du 2nd degré seulement.

## Mes objectifs -------------

# Je suis toujours en train de découvrir les API. Mon premier objectif est d'extraire les données IPS depuis l'API de data.education.gouv.fr.
# Au lieu d'extraire l'ensemble des données, je veux me limiter à certains territoires, pour utiliser plus de fonctions de {httr2}.

# Après...j'observe et j'analyse !

library(httr2)
library(tidyverse)


## IPS dans les écoles primaires -----------
req_ips_cat_ecoles <- request("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets") |> 
  req_url_path_append("fr-en-ips-ecoles-ap2022") |> 
  req_url_path_append("records") |>
  req_url_query(limit = 10) |> 
  req_perform()

req_ips_cat_ecoles |> resp_body_json(simplifyVector = TRUE)

req_ips_data_ecoles <- request("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets") |> 
  req_url_path_append("fr-en-ips-ecoles-ap2022") |> 
  req_url_path_append("exports/json") |>
  # je réduits le nb de colonne et je limite au val-de-marne (rpz)
  req_url_query(
    select = c("rentree_scolaire", "nom_de_l_etablissement as lb_etab", # ya le e d'établiss*e*ment ici
               "code_insee_de_la_commune as code_com", "nom_de_la_commune as lb_com", 
               "secteur", "effectifs", "ips"),
    .multi = "comma"
  ) |> 
  req_url_query(where = "code_du_departement LIKE '094'") |> 
  req_perform()

ips_ecoles <- req_ips_data_ecoles |> 
  resp_body_json(simplifyVector = T) |> 
  as_tibble() |> 
  arrange(code_com)

glimpse(ips_ecoles)

ips_ecoles |> 
  filter(code_com == "94079") |> 
  arrange(rentree_scolaire, ips) |> 
  view("vsm écoles")
# pas surprise par Jean Renon
# par contre, Leon Dauer 2e école la plus "défavorisée", ça me surprend !

## IPS dans les collèges ------------

### 1) observations initiales -------
# le endpoint "records" permet de voir un échantillon des données voulues (de ce que j'ai vu, il y a toujours une limite dans le nombre d'obs, du coup ça me permet d'établir mes filtres pour quand je voudrais les données plus exhaustives)
req_ips_catalog <- request("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets/fr-en-ips-colleges-ap2022/records?limit=20") |> 
  # j'ai copié l'URL par défaut de l'appel API, du coup c'est limité à 20, mais si je faisais à la mano
  req_url_query(limit = 10) |> 
  # en plus ça écrase l'input par défaut, woah
  req_perform()

# vérif OK
req_ips_catalog$status_code == 200  

# extraction des résultats, applatissement direct 
req_ips_catalog |> 
  resp_body_json(simplifyVector = T)

### 2) extraction des données -------------
# je sépare un peu plus l'url, pour après avec les IPS lycées 
req_ips_data <- request("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets") |> 
  req_url_path_append("fr-en-ips-colleges-ap2022") |> 
  # endpoint exports, au format json (apparemment ya du parquet)
  req_url_path_append("exports/json") |>
  # je réduits le nb de colonne et je limite au val-de-marne (rpz)
  req_url_query(
    select = c("rentree_scolaire", "academie", "nom_de_l_etablissment as lb_etab", 
               "code_insee_de_la_commune as code_com", "nom_de_la_commune as lb_com", 
               "secteur", "effectifs", "ips", "ecart_type_de_l_ips as ecart_type_ips"),
    .multi = "comma"
    ) |> 
  req_url_query(where = "code_du_departement LIKE '094'") |> 
  req_perform()

# NB: j'étais sur le point d'abandonner le renommage en amont parce que ERROR, sauf qu'en fait...IL Y A UNE ERREUR DANS LE NOM DE COL, voila 

# en format tabulaire 
ips_colleges <- req_ips_data |> 
  resp_body_json(simplifyVector = T) |> 
  as_tibble() |> 
  arrange(code_com)

glimpse(ips_colleges)

### 3 ) exploration ----
summary(ips_colleges$ips) # rappel : min = défavorisé
summary(ips_colleges$ecart_type_ips) # rappel : min = établissement homogène socialement

#### IPS ----

# 20 indices de position sociale les plus faibles (collèges origines sociales défavorisées)
ips_colleges |> 
  slice_min(order_by = ips, n = 20) |> 
  select(lb_com, lb_etab, secteur, effectifs, ips, ecart_type_ips) |> 
  view("ips faible")
## que du public
## Vitry, Villeneuve-St-Georges & Champigny reviennent plusieurs fois
## plutôt ouest du 94
## sur l'hétérogénéité : max 29.5, en dessous de la moyenne. pas très diversifié 

# 20 indices de position sociale les plus élevés (collèges origines sociales favorisées)
ips_colleges |> 
  slice_max(order_by = ips, n = 20) |> 
  select(lb_com, lb_etab, secteur, effectifs, ips, ecart_type_ips) |> 
  view("ips élevé")
## 12 privé sous contrat, 8 public (rappel 94 : 23 collèges privé ; 107 public)
## Vincennes, St-Maur, St-Mandé, Nogent...tmtc...
## à part Bry et Vitry, littéralement que les villes au nord de la Marne (rive droite de la Marne)
## HAH, et le collège le plus favorisé du Val-de-Marne est aussi le moins mixte. TOP.

#### Hétérogénéité établissements ----
ips_colleges |> 
  slice_min(order_by = ecart_type_ips, n = 20) |> 
  select(lb_com, lb_etab, secteur, effectifs, ips, ecart_type_ips) |> 
  view("mixité faible")

ips_colleges |> 
  slice_max(order_by = ecart_type_ips, n = 20) |> 
  select(lb_com, lb_etab, secteur, effectifs, ips, ecart_type_ips) |> 
  view("mixité forte")
# que du public, vive le public
# géographiquement, c'est assez divers 

#### Villiers-sur-Marne ----
ips_colleges |> 
  filter(code_com == "94079") |> view("vsm")
## IPS Prunais > IPS PMC 
## les deux dans moitié inférieure IPS, moitié supérieur écart type 

#### Graphiques ----
# Hétérogénéité de l'établissement x moyenne origines sociales
ips_colleges |> 
  ggplot(aes(x = ips, y = ecart_type_ips, colour = secteur)) +
  geom_point()
# établissements "défavorisés" comme "favorisés" très homogènes. mixité sociale faible des deux côtés.
# plus grande répartition parmi les étabs publics

# en fonction des effectifs ?
ips_colleges |> 
  ggplot(aes(x = effectifs, y = ecart_type_ips, colour = secteur)) +
  geom_point()
# visuellement, c'est pas concluant 


## IPS dans les lycées ------------
req_ips_cat_lycees <- request("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets") |> 
  req_url_path_append("fr-en-ips-lycees-ap2022") |> 
  req_url_path_append("records") |>
  req_url_query(limit = 10) |> 
  req_perform()

req_ips_cat_lycees |> resp_body_json(simplifyVector = TRUE)
# indicateurs déclinés selon lycée général ou pro

req_ips_data_lycees <- request("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets") |> 
  req_url_path_append("fr-en-ips-lycees-ap2022") |> 
  # endpoint exports, au format json (apparemment ya du parquet)
  req_url_path_append("exports/json") |>
  # je réduits le nb de colonne et je limite à l'académie de Lyon 
  req_url_query(
    select = c("rentree_scolaire", "academie", "nom_de_l_etablissment as lb_etab", 
               "type_de_lycee as type_lycee",
               "code_insee_de_la_commune as code_com", "nom_de_la_commune as lb_com", 
               "departement as lb_dep",
               "secteur", "effectifs_voie_gt", "effectifs_voie_pro", "effectifs_ensemble_gt_pro", 
               "ips_voie_gt", "ips_voie_pro", "ips_ensemble_gt_pro", 
               "ecart_type_de_l_ips_voie_gt as ecart_type_ips_gt",
               "ecart_type_de_l_ips_voie_pro as ecart_type_ips_pro"),
    .multi = "comma"
  ) |> 
  req_url_query(where = "academie like 'LYON'") |> 
  req_perform()


ips_lycees <- req_ips_data_lycees |> 
  resp_body_json(simplifyVector = T) |> 
  as_tibble() |> 
  arrange(code_com)

glimpse(ips_lycees)

# 200 lycées sous académie de Lyon 
ips_lycees |> 
  janitor::tabyl(secteur, type_lycee) |> 
  janitor::adorn_totals(c("row", "col")) |> 
  janitor::adorn_percentages("col") |> 
  janitor::adorn_pct_formatting() |> 
  janitor::adorn_ns()
# ! majorité relative des lycées généraux-technos sont PRIVES 

summary(ips_lycees$ips_ensemble_gt_pro)
summary(ips_lycees$ips_voie_gt)
summary(ips_lycees$ips_voie_pro) #alors déjà...les moyennes IPS en voie pro sont généralement plus basse qu'en général-techno...médiane de 89 v 117!!!

cli::cli_h3("hétérogénéité des établissements - écart-type IPS")
summary(ips_lycees$ecart_type_ips_gt)
summary(ips_lycees$ecart_type_ips_pro)
# plus homogène ENTRE les groupes (diff d'AU SEIN du groupe, attention)

### IPS --------------
# les pauvres
ips_lycees |> 
  slice_min(ips_ensemble_gt_pro, n = 20) |> 
  view("ips faible")
# que des lycées pro ou polyvalent. je vais voir sur les gt seulement...
ips_lycees |> 
  slice_min(ips_voie_gt, n = 20) |> view("ips faible gt")
ips_lycees |> 
  slice_min(ips_voie_pro, n = 20) |> view("ips faible pro")

# les riches 
ips_lycees |> 
  slice_max(ips_ensemble_gt_pro, n = 20) |> view("ips fort")
# LE PRIVEEEEEEEEEEE + Lyon qui truste 16 places quand même (5e arrondissement). Full Rhône 
# par dept ?
ips_lycees |> slice_max(ips_ensemble_gt_pro, n = 5, by = lb_dep) |> view()
# le 69 écrase le reste, c'est dingue
ips_lycees |> slice_max(ips_voie_gt, n = 20) |> view("ips fort gt")
# Jeanne d'Arc (Cessy!! et non Gex) no.7
# hmm je vais être cynique. IPS ensemble Jeanne d'Arc "tiré vers le bas" par la voie pro. 
# seul lycée hors Rhône d'ailleurs 
ips_lycees |> slice_max(ips_voie_pro, n = 20) |> view("ips fort pro")

### Mixité sociale -------------------
ips_lycees |> slice_min(ecart_type_ips_gt, n = 20) |> view("mixité faible gt")
# :) ma foi, ils pratiquent bien l'entre-soi dans le privé de l'académie de lyon :)
# 2 lycées publics polyvalents dans le mix. 
ips_lycees |> slice_min(ecart_type_ips_pro, n = 20) |> view("mixité faible pro")
# lycées vocationnels (coiffure, électrotechnique, service, art, accompagnement à la personne...)

ips_lycees |> slice_max(ecart_type_ips_gt, n = 20) |> view("mixité forte gt")
# RAS
ips_lycees |> slice_max(ecart_type_ips_pro, n = 20) |> view("mixité forte pro")


### LE ZERO UN -------------------
# Ferney-Voltaire apparait NULLE PART je suis choquée
ips_lycees |> 
  filter(lb_dep == "AIN") |> # 26 lycées
  view("aindinois")
# ah bah par contre on a le plus de têtes du 01, youpi (on a le plus de tête de l'académie même !)
# 3e lycée filière GT le plus mixte du département, i'll take that 
# putain, j'ose pas imaginer l'ambiance richouille dans les lycées lyonnais alors que je pensais que Ferney était bien bourge. on n'est même pas dans le top 20 de l'académie !!!! (on est quand même dans le dernier quintile en ips global sur l'académie, bon)

### Graphiques --------------------------
# plusieurs dimensions quand même : géographique, secteur, type de lycée...j'ai la flemme 
ips_lycees |> 
  ggplot(aes(x = ips_voie_gt, y = ecart_type_ips_gt, colour = secteur)) +
  geom_point()
# ouais
ips_lycees |> 
  ggplot(aes(x = ips_voie_gt, y = ecart_type_ips_gt, colour = secteur)) +
  geom_point() +
  facet_wrap(~ lb_dep)

ips_lycees |> 
  ggplot(aes(x = ips_ensemble_gt_pro, y = effectifs_ensemble_gt_pro, colour = type_lycee)) +
  geom_point()
# meh
