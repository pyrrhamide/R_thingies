# on tente un truc : scrapper legifrance pour récupérer les données sur les nouveaux chevaliers/officiers de la légion d'honneur et de l'ordre national du mérite, suite aux jeux olympiques et paralympiques de paris
# deux pages de décrets : 
# https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000050251623 -- pour l'ONM
# https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000050251619 -- pour la LH

# objectif : faire une base propre :)

# packages ----
library(rvest)
library(tidyverse)
library(here)

# on charge les pages ----
lien_om <- "https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000050251623"
lien_lh <- "https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000050251619"

om_raw <- read_html(lien_om)
lh_raw <- read_html(lien_lh)

# faut inspecter la page avant de faire quoique ce soit hmmm 
# que des p :/ aucune class, aucun id, eg pour distinguer les commandeurs (bon, yen a qu'un) des officiers des chevaliers 
om1 <- om_raw |> 
  html_elements("p") |> 
  html_text2() # au lieu de html_text() qui "nettoie" un peu trop à mon goût

om2 <- om1[7:12] # que les lignes d'intérêt
om2

# grades ?
om2[c(T,F)] |> str_squish() # nice, faut juste enlever "Ministère des sports" etc.

# les champions :)
om_noms <- om2[c(F,T)] |> str_flatten() |> str_split_1("\n") 

# test 
head(om_noms)
test <- tibble(phrase = head(om_noms),
               titre = str_extract(phrase, "\\w+"),
               prenoms = str_match(phrase, "\\((.*?)\\)")[,2],
               nom = str_match(phrase, "\\b\\w+\\b\\s*(.*?)\\(")[,2],
               an_service = str_match(phrase, ";\\s*(.*)")[,2])
# on va se limiter à ça. merci chat-gpt pour les regex oh my days 

# ordre national du mérite ----
# att. pour associer le grade à la personne, jvais test un ptit set_names et tout 
om_grades <- om2[c(T,F)] |> str_squish() |> str_remove("Ministère des sports, de la jeunesse et de la vie associative ")

om_init <- om2[c(F,T)] |> 
  set_names(om_grades) |> 
  map(~ str_split_1(.x, "\n")) |> 
  # list_rbind fonctionne pas donc je passe par enframe()
  enframe(name = "grade", value = "phrase") |> 
  unnest_longer(col = phrase) # magueuuuule 

ordre_national_merite0 <- om_init |> 
  filter(phrase != "") |> 
  mutate(grade = str_remove(grade, "Au grade (de\\s|d')")) |> 
  # et puis le reste 
  mutate(titre = str_extract(phrase, "\\w+"),
         prenoms = str_match(phrase, "\\((.*?)\\)")[,2],
         nom = str_match(phrase, "\\b\\w+\\b\\s*(.*?)\\(")[,2],
         an_service_txt = str_match(phrase, ";\\s*(.*)")[,2]) |> 
  # nettoyages 
  mutate(nom = str_remove(nom, "\\.") |> str_trim(),
         an_service = parse_number(an_service_txt)) |> 
  # ok, plusieurs prénoms à séparer 
  mutate(prenom = str_split(prenoms, ", ")) |> 
  unnest_wider(prenom, names_sep = "_") |> 
  # maintenant, séparer nom de naissance et nom d'usage 
  mutate(nom_usage = str_split_i(nom, ",", 1),
         nom_naissance = str_split_i(nom, ", (né|née) ", 2)) |> 
  mutate(athlete = if_else(str_detect(phrase, "Paralympiques"), "paralympique", "olympique"),
         guide = str_detect(phrase, "guide"))

glimpse(ordre_national_merite0)

ordre_national_merite <- ordre_national_merite0 |> 
  select(grade, titre, 
         nom_usage, nom_naissance, starts_with("prenom_"),
         athlete, guide, an_service) |> 
  mutate(ordre = "mérite", .before = 1)

# pareil pour la légion d'honneur :) ----
lh1 <- lh_raw |> 
  html_elements("p") |> 
  html_text2() # au lieu de html_text() qui "nettoie" un peu trop à mon goût

lh2 <- lh1[7:8] # que les lignes d'intérêt
lh2

lh_grades <- lh2[c(T,F)] |> str_squish() |> str_remove("Ministère des sports, de la jeunesse et de la vie associative ")

lh_init <- lh2[c(F,T)] |> 
  # on donne le nom du grade aux éléments du vecteur
  set_names(lh_grades) |> 
  # une ligne = un athlète
  map(~ str_split_1(.x, "\n")) |> 
  # list_rbind fonctionne pas donc je passe par enframe() pour constuire df 
  enframe(name = "grade", value = "phrase") |> 
  # col phrase est une liste, on la "déplie" sur la longueur. grade sera répété (j'adore le tidyverse bordel)
  unnest_longer(col = phrase) # magueuuuule 

legion_honneur0 <- lh_init |> 
  filter(phrase != "") |> 
  mutate(grade = str_remove(grade, "Au grade (de\\s|d')")) |> 
  # et puis le reste 
  mutate(titre = str_extract(phrase, "\\w+"),
         prenoms = str_match(phrase, "\\((.*?)\\)")[,2],
         # nom = str_extract(phrase, "[M\\.|Mme] (\\w+)", group = T),
         nom = str_match(phrase, "\\b\\w+\\b\\s*(.*?)\\(")[,2],
         an_service_txt = str_match(phrase, ";\\s*(.*)")[,2]) |> 
  # nettoyages 
  mutate(nom = str_remove(nom, "\\.") |> str_trim(),
         an_service = parse_number(an_service_txt)) |> 
  # ok, plusieurs prénoms à séparer 
  ## en une fonction :)
  # separate_wider_delim(prenoms, delim = ", ", names_sep = "_", too_few = "align_start") |> 
  ## comment j'ai fait en self 
  mutate(prenom = str_split(prenoms, ", ")) |> 
  unnest_wider(prenom, names_sep = "_") |> 
  # maintenant, séparer nom de naissance et nom d'usage 
  mutate(nom_usage = str_split_i(nom, ",", 1),
         nom_naissance = str_split_i(nom, ", (né|née) ", 2)) |> 
  mutate(athlete = if_else(str_detect(phrase, "Paralympiques"), "paralympique", "olympique"),
         guide = str_detect(phrase, "guide"))

glimpse(legion_honneur0)

legion_honneur <- legion_honneur0 |> 
  select(grade, titre, 
         nom_usage, nom_naissance, starts_with("prenom_"),
         athlete, guide, an_service) |> 
  mutate(ordre = "légion d'honneur", .before = 1)

# ça peut être généralisé en une fonction, de lh/om_grades jusqu'à legion_honneur/ordre_merite en soit 

# bon, on empile !
promus24 <- bind_rows(legion_honneur, ordre_national_merite) 
