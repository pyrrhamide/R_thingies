
# https://linogaliana.netlify.app/post/datatable/datatable-nse/

# je veux créer des fonctions persos avec un data.table, avec choix de
# la variable à l'input de la fonction. 

# ici, je fais un truc tout simple de compter le nombre d'observation par 
# mois de l'année (cas que je rencontre au taff, je répète la même OP
# plusieurs fois, avec juste la table et la variable de date qui changent). 
# j'ai jamais réussi à en faire une fonction, parce que ça arrêtait pas de
# dire que l'objet date n'existait pas.

# le souci est que j'indiquais pas dans la fonction que mon entrée de variable
# de date était bien une colonne du dt ! 
# et la solution...c'est get()...

library(data.table)
library(lubridate)

# liste des films du Marvel Cinematic Universe
mcu <- as.data.table(openintro::mcu_films) # date format chr (en plus c'est le format US, crade)
mcu[, dt_release := lubridate::mdy(release_date)]

# liste des chansons qui sont entrées dans le classement Billboard
bb <- as.data.table(tidyr::billboard)
bb_red <- bb[, artist:date.entered] # on enlève les colonnes par semaine

## sidetrack : bb en format long 
bb_long <- melt(bb, measure = patterns("^wk"), variable.name = "week", value.name = "rank")
bb_long[order(artist, track, week)]
bb_long[, wk_num := stringr::str_remove(week, "wk")]

# fonction simploche qui compte le nombre de lignes par mois 
n_mois <- function(dt, var_date){
  dt[, .N, keyby = lubridate::month(get(var_date), label = T)]
}

n_mois(mcu, "dt_release")
n_mois(bb_red, "date.entered") # et non test(bb_red, date.entered)

# fonction qui compte le nombre de lignes par an et par mois 
n_anmois <- function(dt, var_date){
  dt[, .N, keyby = .(year(get(var_date)), month(get(var_date), label = T))]
}

n_anmois(mcu, "dt_release")
n_anmois(mcu, dt_release)

n_anmois(bb_red, "date.entered")
dcast(n_anmois(bb_red, "date.entered"), year ~ month, value.var = "N", fill = 0)

# FIN - ce que je veux !
eff_anmois <- function(dt, var_date){
  tab_long <- dt[, .N, 
                 keyby = .(year(get(var_date)), 
                           month(get(var_date), label = T))
                 ]
  
  dcast(tab_long, year ~ month, value.var = "N", fill = 0)
}

eff_anmois(bb_red, "date.entered")
# plus rapide, sauf que pas pertinent pour mon cas (lc_sas)
dcast(bb_red, year(date.entered) ~ month(date.entered, label = T))
