
library(data.table)

metro <- as.data.table(gt::metro)
metro

# on a les lignes de métro qui s'arrêtent à chaque station, les RER, les trams, les transiliens, les autres, le nb de passagers (par an ?), latitude et longitude de la station, arrondissement de la station

# il y a même le détail des petites lignes après les stations ! (pas énoncé par la voix quand t'arrives à la station, mais écrit en dessous du panneau)
# les trucs de touristes
metro[!is.na(caption), .(name, caption)]
metro[!is.na(caption), .N, location][order(-N)] # 3 à Créteil??

# pour une station, si elle contient plusieurs lignes, celles-ci sont écrites "1, 2, 3" dans une même colonne (pareil pour les RER/trams/transiliens). on change ça en une colonne par ligne
metro[, paste0("metro_", 1:5) := tstrsplit(lines, ",")]
metro[, paste0("rer_", 1:3) := tstrsplit(connect_rer, ",")]
metro[, paste0("tram_", 1:2) := tstrsplit(connect_tramway, ",")]
metro[, paste0("transilien_", 1:2) := tstrsplit(connect_transilien, ",")]

names(metro)

# test - somme nb métro
metro[, lapply(.SD, \(x) fifelse(!is.na(x),T,F)), .SDcols = names(metro) %like% "^metro_"][, .(n=rowSums(.SD))]
