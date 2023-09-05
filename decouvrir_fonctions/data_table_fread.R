# encore du data.table, j'ai besoin d'appreeeeendre 

# https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv

library(data.table)

# on a téléchargé le fichier de données des cas de covid aux USA. on suppose que c'est mastoc, donc au lieu de tout importer d'un coup, ou d'ouvrir le fichier sur Excel, on importe seulement les 10 premières lignes!
dt_s <- fread("decouvrir_fonctions/data/us-counties.csv", nrows = 10)
dt_s

# mais ! on peut aussi importer 0 ligne, et n'avoir que les noms de colonnes !
dt_vars <- fread("decouvrir_fonctions/data/us-counties.csv", nrows = 0)
names(dt_vars)
# pk j'ai jamais pensé à ça avant d'importer des tables Genesis entières et torturer mon PC...

# une fois qu'on sait quelles variables nous intéressent, on peut les sélectionner directement à l'import
## soit en les nommant directement
dt <- fread("decouvrir_fonctions/data/us-counties.csv", 
            select = c("date", "county", "state", "cases"))

## soit en donnant leur index 
dt <- fread("decouvrir_fonctions/data/us-counties.csv",
            select = c(1:3, 4))

## on peut également créer un vecteur dans l'environnement R, des variables qu'on veut importer
my_cols <- c("date", "county", "state", "cases")
dt <- fread("decouvrir_fonctions/data/us-counties.csv",
            select = my_cols, nrows = 10) # 2 mio de lignes fraté

## à l'opposé de select, on a drop: importer toutes les colonnes, sauf celles-ci!
dt <- fread("decouvrir_fonctions/data/us-counties.csv",
            drop = c("fips", "deaths"), nrows = 20)
# ou avec index
dt <- fread("decouvrir_fonctions/data/us-counties.csv",
            drop = c(4, 6))

## ??? tu peux sélectionner direct des observations avec grep ???
dt_cal <- fread("grep California decouvrir_fonctions/data/us-counties.csv")
dt_cal # 1) punaise, c'est magique ; 2) tu perds les noms de colonnes ?
dt_cal[, unique(V3)]

# ah, tu peux récuperer les noms de colonnes avec eg la table vide 
dt_cal <- fread("grep California decouvrir_fonctions/data/us-counties.csv",
                col.names = names(dt_vars))
dt_cal # en effet

## ...on va encore plus loin dans les expressions régulières à l'import. plusieurs états d'un coup
# apparemment, ce sont des commandes qui fonctionnent dans l'invite de commande aussi (grep je veux dire)
states4 <- fread(cmd = "grep -E 'Oregon|Montana|Maine|Kentucky'  decouvrir_fonctions/data/us-counties.csv", col.names = names(dt_vars)) # ça fonctionne pas :(

# bon, même si ça fonctionne, c'est pas efficace parce que ça cherche dans toutes les colonnes, pas seulement la colonne "states". si on avait 80 colonnes (tmtc), ce serait un peu chaud.

## définir type des colonnes dans l'import, pour quelques unes pas toutes
dt <- fread("decouvrir_fonctions/data/us-counties.csv",
            nrows = 20, colClasses = c("date" = "Date")) # nom col = type col

## sa mère. fread() peut importer des fichiers zippés! .zip et .gz
# .gz direct dans fread()
# .zip dézippé dans l'appel de la table, eg:
fread(cmd = "unzip -cq table.zip")

