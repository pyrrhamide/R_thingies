### DECOUVRIR ET NETTOYER UNE BASE DE DONNEES ----

# 1.Pour obtenir info/résumé sur data frame ou variable ----
names(df)
str(df)
glimpse(df)
lookfor(df)

table(df$variable, useNA = "ifany")
table(df$old_var, df$new_var, useNA = "ifany") # tri croisé

summary(df) # stats de base
summary(df$variable)

unique(df$variable) # permet de voir modalités d'une variable
describe(df$variable)

which(is.na(df$variable)) # pour trouver position de NAs dans variable

colnames(df)
rownames(df)

head(base$var, 50) # afficher les 50 premières obs de la var

# 2.Manipuler le data frame ----
  # basique
rm(variable)
rm(base)

base$var <- NULL

  # par l'indexation
df <- df[,-c(14:25)] # supprimer colonnes/variables de 14e à 25e position dans df (à vérifier avec lookfor)
df <- df[-c(14:25),] # supprimer LIGNES de 14e à 25e position
df <- df[,c(14:25)]  # GARDER colonnes
df <- df[c(14:25),]  # GARDER lignes 14 à 25
df <- df[1:10,1:3]   # Les 10 premiers individus, seulement les premières colonnes

base[1,3] # 1ère ligne/observation, 3ème colonne/variable

base[base$var == "modalité recherchée",] # garder les lignes où la var vaut "moda recherchée"

  # par subset
subset(base, var == "moda recherchée") # /!\ subset enlève les NAs
subset(base, var == "moda", select = c(var1, var2)) # select = garder seulement ces 2 colonnes/variables
subset(base, var == "moda", select = -c(var1, var2)) # -c = tout SAUF ces 2 colonnes/variables
subset(base, select = -c(var1, var2)) # sans condition de var

  # rassembler des dataframes / fusion
    # solution 1 : pas la best
cbind(base1, base2) # pour nb colonnes égal
rbind(base1, base2)
    # solution 2 : merge
merge(base1, base2, by = "clé de fusion") # clé pas obligatoire
merge(base1, base2, by = c("clé 1", "clé 2"))
merge(base1, base2, by.x = "clé base1", by.y = "clé base 2") # si clé nom différent ds chaque base
merge(base1, base2, by = "clé", all.x = T, all.y = T)

  # trier un dataframe
order(c(2,19,8)) # Seule, la fonction renvoie l'ordre des modalites
order(c(2,19,8), decreasing = T) # Par ordre decroissant

base[order(base$var),] # ordre croissant par défaut
base[order(base$var1, base$var2),]

  # DOUBLOOOOONS
base.fusion <- base.fusion[order(base.fusion$var),] # trier en fonction de la var qui nous interesse
doublons <- which(duplicated(base.fusion$var)) # créer vecteur indiquant position des doublons en fonction var
df.doublons <- base.OG[doublons,]
base.fusion <- base.fusion[-doublons,] # enlever les doublons

# 3.Préparer la base et les variables ----
  # 3.1 Nommer les modalites
base$new_var <- factor(base$old_var, levels=c("label 1","label 2")) # Factor pour transformer en facteur, label pour nommer les variables

base$new_var[base$old_var == "moda1"] <- "label 1"
base$new_var[base$old_var %in% c("moda2", "moda3")] <- "label 2"
irec(base$old_var)

  # 3.2 Renommer une variable
base <- rename.variable(base, "old_varname", "new_varname")

  # 3.3 Transformer une variable numérique
base$new_var <- cut(base$old_var, 3) # New var en 3 modalités
base$new_var <- cut(base$old_var, c(16,20,25,30,35,40,45,50,55,100)) # Spécifier où se trouver la coupe
base$new_var <- cut(base$old_var, c(16,20,25,30,35,40,45,50,55,100), include.lowest = T)
base$new_var <- cut(base$old_var, c(15,20,25,30,35,40,45,50,55,100), include.lowest = T,
                       right = F)
base$new_var <- cut(base$old_var, c(15,17,18,20,22,24,26,28,30,100), include.lowest = T,
  labels = c("Moins de 18","18 ans","19-20","21-22","23-24","25-26","27-28","29-30","Reprise d'etudes"))  # donner label directement
icut(base, old_var)

  # 3.4 quantiles
base$new_var <- quant.cut(base$old_var, 4)


# 4.Regrouper des modalités ----
  # 4.1 par indexation
base$newvar[base$oldvar == "label"] <- "new label"
  # 4.2 irec
irec(base$var)
  # 4.3 ifelse => peut s'imbriquer
      # /!\ si imbrication, partir du cas le plus particulier au cas le plus général
base$newvar <- ifelse(base$oldvar == "condition", "valeur si vrai", "valeur si faux")
base$newvar <- ifelse(base$var1 == "ci" & base$var2 == "ça", "valeur si vrai", "valeur si faux")
  # 4.4 case_when
base$newvar <- case_when(base$oldvar == "condition" ~ "valeur si vrai",
                         base$oldvar2 =="condition" ~ "valeur si vrai",
                         TRUE ~ "valeur si faux") # TRUE pour indiquer tout le reste


# 5.Ordonner les modalités => pour ordonner moda, var doit être facteur ----
base$var <- as.factor(base$var)
base$var <- fct_relevel(base$var, "label 1", "label 2", "label 3")
  # ou : indexation avec chiffre devant
base$newvar[base$oldvar == "label 1"] <- "1. New label"
base$newvar[base$oldvar == "label 2"] <- "2. New label"
base$newvar[is.na(base$oldvar) == T] <- NA


# 6.Coller des variables ----
  # paste
base$newvar <- paste(base$var1, base$var2)
base$newvar <- paste(base$var1, base$var2, sep="+")

base$newvar <- paste(base$var1, base$var2, base$var3, sep="-")

  # collapse : pour concaténer moda variables
paste(base$var, collapse = ",")

  # interaction : donner l'ensemble de combinaison de 2+ variables
interaction(base$var1, base$var2)

# 7.Conversion des variables ----
factor(base$var)
as.character(base$var)
as.numeric(base$var)
as.numeric(as.character(base$var))

# 8.Travailler texte des variables ----
  # Majuscules et minuscules
str_to_lower(base$var) # tout en minuscule
str_to_upper(base$var) # tout en majuscule
str_to_title(base$var) # maj en début de mot

  # Extraire des sous-chaînes
str_sub(base$var,1,1) # extraire le premier caractère du premier caractère
str_sub(base$var,3,5) # caractères 3 à 5

  # Détecter des motifs
str_to_lower(base$var)
str_detect(base$var, "mot")
    # ou (sans changer casse)
str_detect(base$var, regex("mot", ignore_case = T))
    # str_count pour nb fois chaîne de caractère apparait
str_count(base$var, "mot")
    # str_subset pour extraire obs qui évoquent chaîne de caractère
str_subset(base$var, "mot")

  # Remplacer les motifs
str_replace(base$var, "Mot à remplacer", "remplacement")
    # plusieurs remplacements à la fois
str_replace_all(base$var, c("Mot 1" = "Remplacement 1", "Mot 2" = "Remplacement 2"))

# 9.Création codebook ou dictionnaire des codes
library(dataMaid)
makeCodebook(base, replace = T)

# Importer données ----
df <- read.csv2("kfiaf/donnees.csv", encoding = "UTF-8")
# avec work Directory
setwd("work/directory")
df <- read.csv2("donnees.csv")
# read_spss("donnees.sav") # package haven
# read_sas("donnees.sas7bdat") # package haven
# read_dta / read_stata pour STATA
# read_excel

# Créer un rapport HTML avec quelques stats directes ----
library(DataExplorer)
create_report(df)
