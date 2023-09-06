library(tidyverse)

starwars
guerre <- as.data.table(starwars)

head(guerre)
glimpse(guerre)

guerre[, lapply(.SD, class)]
guerre[, lapply(.SD, uniqueN)] # no bueno, je suppose que c'est à cause des trois colonnes-listes ? réponse = non...

guerre[, lapply(.SD, unlist), .SDcols = c('films', 'vehicles', 'starships')]

guerre[, unlist(films), name][, .N, V1][order(-N)] # attack of the clones a le plus de personnages, suivi de revenge of the sith, puis the phantom menace
guerre[, unlist(films), name][, .N, name][order(-N)] # R2-D2 number one! C-3PO number two!! Obi-Wan Kenobi number three!!!
