
# test pour 
# - générer toutes les années comprises entre une année min et une année max, 
# - séparer en x nombre de colonnes, 
# - pivoter, puis compteeeer

# packages
library(data.table)
library(purrr)
library(splitstackshape)

# exemple
dt <- data.table(
  x = LETTERS[1:25],
  andeb = rep(c(2017, 2018, 2020, 2018, 2022), each = 5),
  anfin = rep(c(2018, 2018, 2022, 2019, 2023), 5)
)

map(dt, \(x) print(x))

# dt[, seq_between := apply(.SD, 1, function(row) seq(row["andeb"], row["anfin"])), .SDcols = c("andeb", "anfin")]

dt[, years := apply(.SD, 1, 
                    function(row) 
                      toString(seq(row["andeb"], row["anfin"]))
                    ), .SDcols = c("andeb", "anfin")][]
# ! ajouter [] à la fin permet d'afficher le datatable après opération ! 
# à la (x <- y)

dt[, lapply(.SD, class)]
dt[, tstrsplit(years, ",")] 

dt |> rowwise() |> mutate(z = length(seq(andeb, anfin)))

# sépare les années, en **ligne** ! (une nouvelle année par identifiant = une nouvelle ligne) => simplifie ma vie!
cSplit(dt, "years", sep = ',', direction = 'long', type.convert = (as.is = F))
# sépare les années, en **colonne** (une nouvelle année par identifiant = une nouvelle colonne, nom automatique)
cSplit(dt, "years", sep = ",", direction = "wide", type.convert = (as.is = F))


fin <- cSplit(dt[, years, x], 
              "years", sep = ',', direction = 'long', 
              type.convert = (as.is = F))

fin
fin[, .N, keyby = years] # huzzah
