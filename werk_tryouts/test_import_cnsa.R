
# CNSA - test technique

library(tidyverse)
library(janitor)

# https://www.data.gouv.fr/fr/datasets/prix-hebergement-et-tarifs-dependance-des-ehpad-donnees-brutes/
# Prix hébergement et tarifs dépendance des EHPAD - données brutes
# (données retravaillées ailleurs)

# MAD des données au 31/12 de l'année passée, ou au dernier jour du mois précédent pour l'année en cours 
# par curiosité, on va regarder les deux types (a l'air d'y avoir les mêmes colonnes selon préviz)
prix_202405_url <- "https://www.data.gouv.fr/fr/datasets/r/e04c8d5e-9287-4b59-a31a-843729e223b4"

prix_2023_url <- "https://www.data.gouv.fr/fr/datasets/r/909d509b-cb05-40ee-b596-32cea668cd84"

read_csv2(prix_202405_url) |> glimpse() # 4388x40
read_csv2(prix_2023_url) |> glimpse() # 6567x40

prix_2023 <- read_csv2(prix_2023_url) |> 
  clean_names()

glimpse(prix_2023)

prix_2023 |> 
  count(year(date_maj), month(date_maj))

# identifiant EHPAD = clé ?
n_distinct(prix_2023$finess_et)
n_distinct(prix_2023$finess_et) == nrow(prix_2023) # oui

# https://www.data.gouv.fr/fr/datasets/finess-extraction-du-fichier-des-etablissements/
# FINESS extraction du fichier des établissements 

# version avec coord, version sans, version historique 2004-2023
# let's go pour la version standard (au 13/05/2024 à la date de ce prg)
finess_url <- "https://www.data.gouv.fr/fr/datasets/r/2ce43ade-8d2c-4d1d-81da-ca06c82abc68"

zarb <- read_csv2(finess_url) # les noms des colonnes sont dans le fichier PDF, URGH 

zarb2 <- read_delim(finess_url, delim = ";", skip = 1, col_names = F) # très étrange quand même, les noms de colonnes à part...

# j'ai copié le tableau du PDF. character striiiing
x <- "Donnée Balise XML Numéro d’ordre
Section : structureet – 1
Numéro FINESS ET nofinesset 2
Numéro FINESS EJ nofinessej 3
Raison sociale rs 4
Raison sociale longue rslongue 5
Complément de raison sociale complrs 6
Complément de distribution compldistrib 7
Numéro de voie numvoie 8
Type de voie typvoie 9
Libellé de voie voie 10
Complément de voie compvoie 11
Lieu-dit / BP lieuditbp 12
Code Commune commune 13
Département departement 14
Libellé département libdepartement 15
Ligne d’acheminement (CodePostal+Lib commune) ligneacheminement 16
Téléphone telephone 17
Télécopie telecopie 18
Catégorie d’établissement categetab 19
Libelle catégorie d’établissement libcategetab 20
Catégorie d’agrégat d’établissement categagretab 21
Libellé catégorie d’agrégat d’établissement libcategagretab 22
Numéro de SIRET siret 23
Code APE codeape 24
Code MFT codemft 25
Libelle MFT libmft 26
Code SPH codesph 27
Libelle SPH libsph 28
Date d’ouverture dateouv 29
Date d’autorisation dateautor 30
Date de mise à jour sur la structure datemaj 31
Numéro éducation nationale numuai 32"

vec_cols_sep <- str_split_1(x, "\n")[-1] # sans l'en-tête du tableau
# faut garder l'avant-dernier mot. 
# on sépare chaque item par l'espace
t1 <- str_split(vec_cols_sep, "\\s")
t1[[1]][length(t1[[1]])-1] # yeees. pour une meilleure compréhension : de t[[1]], je demande le dernier élement du vecteur à partir de la longueur du vecteur (donc "1"), MOINS 1, donc l'avant dernier élement :)

# pour chaque item de la liste (normalement 32)
zarb2_vec0 <- map_chr(seq_along(t1), \(x) t1[[x]][length(t1[[x]])-1])
zarb2_vec0[1] <- "structureet"

names(zarb2) <- zarb2_vec0

zarb2 |> glimpse()
zarb2 |> count(libsph)

n_distinct(zarb2$nofinesset) == nrow(zarb2) # clé unique = nofinesset
