
pacman::p_load(data.table, here, tidyverse)

# http://ergast.com/mrd/db/#csv

# liste de tous les csv de la db F1
liste_csv <- list.files(here("tester_samuser", "f1db_csv"))
# liste des noms de fichiers, sans ".csv"
liste_names <- str_extract(liste_csv, ".*(?=\\.csv)")

# imports seulement des circuits
circuits <- fread(here("tester_samuser", "f1db_csv", "circuits.csv"))

# imports de tout d'un coup
formule_un <- map(
  liste_csv, 
  \(x) fread(here("tester_samuser", "f1db_csv", x), na.strings = "\\N") |> 
    janitor::clean_names()
) |> 
  # sortie liste, on donne un nom = nom fichier -.csv aux élements
  set_names(liste_names)

# vérif des dt qu'on a 
names(formule_un)

# les pilooootes
pilotes <- formule_un$drivers

pilotes[, lapply(.SD, n_distinct)]
pilotes[, lapply(.SD, class)]

# on est chauvin ici 
pilotes[nationality == "French", .(forename, surname)]

# les écuries
ecuries <- formule_un$constructors
glimpse(ecuries)

# les gagnants (pilotes) de chaque course --- FASTEST LAP
driver_fp <- formule_un$results[rank == "1", .N, driver_id][order(-N)]
driver_fp

merge(pilotes, driver_fp)[, -c('url', 'dob')][order(-N)] # ??? Oscar Piastri ??? compte les courses F2 ou bieng ?
# après lecture docu: rank = "1" correspond aux fastest lap remportés ^^ 
# 1-HAM, 2-RAI, 3-VET, 4-VER, 5-ALO

# les gagnants (pilotes) de chaque couse --- P1
driver_win <- formule_un$results[position == "1", .N, driver_id][order(-N)]

merge(pilotes, driver_win)[, -c('url', 'dob')][order(-N)]
# VER va foncedé Prost et Vettel d'ici la fin de la saison 2023...
merge(pilotes, driver_win)[, -c('url', 'dob')][order(-N)][1:15]

# les gagnants (écuries) de chaque course --- P1
constr_win <- formule_un$results[position == "1", .N, constructor_id]

merge(ecuries, constr_win)[, -c('url')][order(-N)][1:10]
# FERRARI!!!!

# plus grand écart entre les wins, par écurie
courses <- formule_un$races[, .(race_id, year, circuit_id, date)]

constr_win_race <- merge(courses, 
      formule_un$results[position == "1", .(race_id, constructor_id)]
      )

constr_win_race[, nb_win := rowid(date), constructor_id][nb_win > 1]

formula_un$results[position == "1"][m]
