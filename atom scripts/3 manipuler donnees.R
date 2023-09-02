####### MANIPULER LES DONNEES 2 ----

droplevels() # pour supprimer unused levels (moda avec 0 obs)

#### [dplyr] ----
base_tibble <- as_tibble(base)

  # traiter les individus avec filter
base_subset <- subset(base, var == "moda")
base_filter <- filter(base, var == "moda") # même résultat

  # traiter les colonnes avec Select
# Permet de choisir des colonnes ou d'en enlever
df.lmd_age <- select(univ, lmd, classe_age_2)
df.pas.lmd_age <- select(univ, -lmd, -classe_age_2) # tout SAUF lmd & classe_age_2

df.sexe.a.lmd <- select(univ,sexe2:lmd)
# On a conservé les variables de sexe2 à lmd

# Select va aussi permettre de bouger une variable en premiere position, avec l'argument everything()
univ <- select(univ, AGE, everything())
univ <- select(univ, ID, sexe2, everything())

  # renommer des variables avec rename
base <- rename(base, new_varname1 = old_varname1,
                     new_varname2 = old_varname2)

# Comme on est dans un "tibble"
# On peut mettre des espaces et des accents dans le nom de la variable
# Même si cela n'est pas franchement conseillé
univ <- rename(univ, "Lit litté classique" = E03a_01)
table(univ$`Lit litté classique`)

  # trier avec arrange
# Permet de trier, à la manière d'order, mais de manière plus pratique
# on trie par var1 puis par var2
base <- arrange(base, var1, var2)
# pour trier par ordre décroissant, desc()
base <- arrange(base, desc(var1), var2)

  # créer une variable avec mutate
base <- mutate(base, newvar = "condition")
base <- mutate(base, newvar = ifelse(var1 == 1, "cat 1",
                                ifelse(var1 == 2, "cat 2","reste")))


#### enchaîner des opérations ----

test <- arrange(select(filter(univ, sexe2 == "Femme"), AGE, sexe2), AGE)
# lecture difficile
temp <- filter(univ, sexe2 == "Femme")
temp <- select(temp, AGE, sexe2)
test2 <- arrange(temp, AGE)
# lourd

# SOLUTION: pipe %>%
test_pipe2 <- univ %>%
  filter(sexe2 == "Femme") %>%
  select(AGE, sexe2) %>%
  arrange(AGE)
  # rep nom base unique!


#### Opérations groupées ----

### a. group_by

# Elle permet de créer des groupes de lignes en fonction des modalités d'autres variables
# Ex : on crée des groupes par fil2
univ %>% group_by(fil2)

# Cela ne change rien de prime abord ...
# ... mais la fonction va ensuite servir pour les autres opérations

# Ici, on va par exemple prendre l'age du plus vieux par fil2 et par sexe
univ %>% group_by(fil2, sexe2) %>%
  arrange(desc(AGE), .by_group = TRUE) %>%
  slice(1) %>%
  select(fil2, sexe2, AGE)
# Je pars d'univ
# Je fais des "groupes fictifs" par filiere d'études et par sexe
# Dans chacun de ces groupes, je trie par age décroissant (et non dans toute le base !)
  # pour Arrange, il faut utiliser l'argument .by_group = TRUE
# Je prends celui en premère position, donc le plus vieux
  # La fonction slice permet de sélectionner(comme select) mais en fonction de la position (ici la position 1)

    # La fonction group.by peut être utilisée avec mutate

# Par exemple, on veut créer une variable de moyenne du travail d'heures par semaine,
#   le tout par filière
# On peut ainsi rapidement voir si l'individu travaille plus que les étudiants de sa filière
univ <- univ  %>% group_by(fil2) %>%
  mutate(moy_trav_fil = round(mean(heures_trav_semaine, na.rm = T),1)) %>%
  mutate(trav_fil = ifelse(is.na(heures_trav_semaine) == T, NA,
                           ifelse(moy_trav_fil > heures_trav_semaine, "Travaille moins","Travaille plus")))

univ <- univ %>% select(fil2, moy_trav_fil, heures_trav_semaine, trav_fil, everything())
table(univ$trav_fil, useNA = "ifany")
# Je pars d'univ
# Je crée des groupes par filière d'études
# Je crée une variable indiquant, pour chaque "groupe" de filière, la moyenne d'heures travaillées par semaine
# Je crée ensuite une variable indiquant si mon individu travaille plus que la moyenne des étudiants de sa filière

# On a créé en quelques lignes 2 variables permettant de savoir si l'individu travaille plus que les étudiants de sa filière

# Si l'on veut dégrouper, il suffit d'appliquer group_by()
univ %>% group_by()
univ <- univ %>% group_by()

# Ou ungroup()
ungroup()

### b. summarise
## La fonction permet d'avoir des infos sur une ou plusieurs colonnes du tableau

# Ex : Age moyen
univ <- univ %>% group_by()
univ %>%
  summarise(Age_moy = mean(AGE, na.rm=TRUE)) # toutes les observations

univ %>% group_by(log) %>%
  summarise(Age_moy = mean(AGE, na.rm=TRUE)) # observations groupées par type de logement

univ %>% group_by(log) %>%
  summarise(mean(AGE, na.rm=TRUE)) # même qu'au-dessus mais sans label

## Summarise permet également de compter le nombre de lignes correspondantes du groupe
# Ex : effectifs
univ %>%
  group_by(fil2) %>%
  summarise(effectifs = n())

# On peut egalement le faire par groupe de 2 variables
# Nombre de filles et de garçons par filière
univ %>%
  group_by(fil2,sexe2) %>%
  summarise(n())

### c. count
# Si on veut juste le nombre de ligne, on peut aussi utiliser count
base %>%
  count(var1, var2, var3)


#### [dplyr] - divers ----

### a. sample_n : permet de choisir nombre de lignes aléatoires (échantillon)
base_sample <- base %>% sample_n(20) # échantillon de 20 individus
    # sample_frac : choisir pourcentage de lignes aléatoires
base_sample <- base %>% sample_frac(0.10) # 10% de "base"

### b. tally
    # équivalent de count quand lancé 1 fois
base %>% group_by(var) %>% tally()

### c. distinct : supprime les éventuels doublons
base2 <- base %>% distinct("var clé identifiante") # garde seulement variable(s) indiquée(s)
base2 <- base %>% distinct(var, .keep_all = T) # supprime doublon dans var, et garde autres vars


#### [TidyR] : ordonner ses données ----

base %>% gather(key = var1, value = new_varname) # rassembler les colonnes/Variables
base %>% spread(key = var1, value = old_varname)
