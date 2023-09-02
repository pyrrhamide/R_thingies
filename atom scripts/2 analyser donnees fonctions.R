### ANALYSER UNE BASE DE DONNEES ----

## 1. Tris simples ---

  # Variables qualitatives
summary(base$var)
    # Attention au format des Variables (e.g code postal)
summary(as.factor(base$var))

freq(base$var)
freq(base$var, cum = TRUE, total = TRUE, digits = 2, exclude = NA)
      # cum = TRUE donne le pourcentage cumule : on a ainsi rapidement les % de licence, etc.
      # total = TRUE pour avoir le total
      # digits = 2 pour avoir 2 chiffres apres la virgule
      # exclude = NA pour exclure les NA (si on veut les pourcentages sans les NA par exemple)
freq(base$var.tab[base$var.filtre == "moda"], cum = TRUE, total = TRUE, digits = 1)

table(base$var, useNA = "ifany")
table(base$var, useNA = "no")
table(base$var, useNA = "always")
    # moins détaillé que freq, mais plus manipulable pour la suite
prop.table(table(base$var, useNA = "ifany")) # proportions
prop.table(table(base$var, useNA = "ifany"))*100 # pourcentages
round(prop.table(table(base$var, useNA = "ifany"))*100, 2) # pourcentages arrondis 2 chiffres après virgule
addmargins(round(prop.table(table(base$var, useNA = "ifany"))*100, 2)) # ajout marge, meme resultat que freq
addmargins(round(prop.table(table(base$var.tab[base$var.filtre == "moda"], useNA = "ifany"))*100,1))

  # Variables quantitatives
## Voir auparavant les fonctions means, quantile, max, min, summary, etc.
    # écart-type
sd(base$var, na.rm = T)
sd(base$var.tab[base$var.filtre == "moda"], na.rm = T)


## 2. Tris croisés ----

  # Entre variables quantitatives
table(base$var1, base$var2, useNA = "ifany")

    # marges
addmargins(table(base$var1, base$var2, useNA = "ifany"))
addmargins(table(base$var1, base$var2, useNA = "ifany"),1) # Marges de la ligne seulement
addmargins(table(base$var1, base$var2, useNA = "ifany"),2) # Marges de la colonne seulement
    # pourcentages
prop.table(table(base$var1, base$var2, useNA = "ifany")) # Pourcentage par rapport a l'ensemble
prop.table(table(base$var1, base$var2, useNA = "ifany"),1) # Pourcentage en ligne (la somme d'une ligne fait 100%)
prop.table(table(base$var1, base$var2, useNA = "ifany"),2) # Pourcentage en colonne

round(prop.table(table(base$var1, base$var2, useNA = "ifany"),1)*100,1) # % en ligne
addmargins(round(prop.table(table(base$var1, base$var2, useNA = "ifany"),1)*100,1),2) # % en ligne
addmargins(round(prop.table(addmargins(table(base$var.tab1[base$var.filtre == "moda"],
                                             base$var.tab2[base$var.filtre == "mode"],
                                             useNA = "ifany"),1),1)*100,1),2)

tab.objet <- addmargins(round(prop.table(table(base$var1, base$var2, useNA = "ifany"),1)*100,1),2)

    # PLUS SIMPLE CEPENDANT
lprop(table(base$var1, base$var2, useNA = "ifany")) # % en ligne
cprop(table(base$var1, base$var2, useNA = "ifany")) # % en colonne

  # Entre une variable qualitative et une variable quantitative

    # apply / lapply / sapply : fonction pour faire plusieurs fois même opération
  apply(base, 2, table)
        # base : à la base "base"
        # 2 : à toutes les colonnes
        # table : la fonction table

    # Tapply applique la meme fonction sur un vecteur A pour chaque modalite d'un vecteur B (ou donc, d'une variable)
tapply(univ$D01_01, univ$sexe2, mean, na.rm = T) # calculer le nb moyen d'heures travaillées en semaine
                                                 # selon le sexe
round(tapply(univ$D01_01, univ$sexe2, mean, na.rm = T),1) # arrondi

# A nouveau, on peut décider d'indexer pour combiner d'autres variables
round(tapply(univ$D01_01[univ$lmd == "Licence"],
             univ$sexe2[univ$lmd == "Licence"], mean, na.rm = T),1)
round(tapply(univ$D01_01[univ$lmd == "Master"],
             univ$sexe2[univ$lmd == "Master"], mean, na.rm = T),1)
round(tapply(univ$D01_01[univ$lmd == "Doctorat et plus"],
             univ$sexe2[univ$lmd == "Doctorat et plus"], mean, na.rm = T),1)

  # Entre deux variables quantitatives - voir séance graphiques

## 3. Significativité de variables quantitatives ----
  # test du khi-deux
tab.objet <- table(base$var1, base$var2, useNA = "ifany")
chisq.test(tab.objet) # existence lien entre variables
chisq.residals(tab.objet) # permet de voir sur/sous-représentation d'individus
    # Si la valeur est inférieure à -2 dans une case, sous-représentation
    # Si superieure à 2, surrepresentation.
    # Si entre -2 et 2, pas d'ecart significatif
  # V de Cramer
cramer.v(tab.objet) # intensité relation
    # - la relation est nulle ou tres faible quand le V est < 0.1
    # - elle est faible quand < 0.2
    # - elle est moyenne quand < 0.3
    # - elle est forte quand > 0.3

## 4. Représentations graphiques de certaines relations ----
    # basique
plot(base$var)
plot(base$var, col ="blue") # OK pour catégories

    # histogram
hist(base$var) # OK pour valeurs continues
hist(base$var, col = "yellow",
     main = "Titre", # Titre
     xlab = "Axe des abcisses",
     ylab = "Axe des ordonnées")

     # boîte à moustache 1 - détaillé
boxplot(base$var, col = grey(0.8), main = "Titre", ylab = "ordonnées")
abline(h = median(base$var, na.rm = TRUE), col = "navy", lty = 2) # fait apparaître ligne médiane
text(1.35, median(base$var, na.rm = TRUE) + 0.15, "Médiane",
     col = "navy") # fait apparaître texte ligne médiane
Q1 <- quantile(base$var, probs = 0.25, na.rm = TRUE) # générer valeur Q1
abline(h = Q1, col = "darkred") # ligne Q1
text(1.35, Q1 + 0.15, "Q1 : premier quartile", col = "darkred", lty = 2) # texte ligne Q1
Q3 <- quantile(base$var, probs = 0.75, na.rm = TRUE) # générer valeur Q2
abline(h = Q3, col = "darkred") # ligne Q2
text(1.35, Q3 + 0.15, "Q3 : troisième quartile", col = "darkred", lty = 2) # texte ligne Q2
arrows(x0 = 0.7, y0 = quantile(base$var, probs = 0.75, na.rm = TRUE),
       x1 = 0.7, y1 = quantile(base$var, probs = 0.25, na.rm = TRUE),
       length = 0.1, code = 3)
text(0.7, Q1 + (Q3 - Q1)/2 + 0.15, "h", pos = 2)
mtext("On a dans la zone grise 50 % des individus",
      side = 1)
abline(h = Q1 - 1.5 * (Q3 - Q1), col = "darkgreen")
text(1.35, Q1 - 1.5 * (Q3 - Q1) + 0.15, "Q1 -1.5 h", col = "darkgreen",
     lty = 2)
abline(h = Q3 + 1.5 * (Q3 - Q1), col = "darkgreen")
text(1.35, Q3 + 1.5 * (Q3 - Q1) + 0.15, "Q3 +1.5 h", col = "darkgreen",
     lty = 2)

     # boîte à moustache 2
boxplot(var.main ~ var.fonction, data = base)
boxplot(var.main ~ var.fonction, data = base, main = "Titre", col = "bisque2")

    # boîte à moustache 3 - pirateplot[yarrr]
pirateplot(var.main ~ var.fonction, data = base,
             main = "Titre",
             xlab = "abcisses",
             ylab = "ordonnées",
             inf.method = "ci", # permet d'avoir l'intervalle de confiance de la moyenne
             bar.f.o = 0.1, bar.f.col = "blue" # Je fais ici varier la couleur en dessous de la moyenne)
          # Les points représentent les données
          # La barre horizontale la moyenne
          # Le rectangle autour de la barre l'intervalle de confiance de la moyenne
          # La forme la distribution

    # nuage de points - 2 vars quanti
plot(base$var1, base$var2)

## 5. Pondération ----
    # [questionr]
wtd.mean(base$variable, weights = base$variable_ponderation) # moyenne pondérée

wtd.table(base$variable, weights = base$variable_ponderation) # tableau pondéré
round(prop.table(wtd.table(base$var, weights = base$var_pond))*100,1) # % pondérés arrondis
addmargins(round(prop.table(addmargins(wtd.table(base$var1, base$var2, weights = base$var_pond),2),2)*100,1),1)
cprop(wtd.table(base$var1, base$var2, weights = base$var_pond))

    # [Survey]
base.pond <- svydesign(ids = ~1, data = base, weights = ~base$var.ponderation)
  # base.pond = tableau de donnees equivalent à base mais qui tient compte des pondérations
base.pond$var
base.pond$variables$var

svytable(~var1 + var2, base.pond)
tab <- svytable(~var1 + var2, base.pond)
cprop(tab)

## 6. Exporter tableaux ----
tab <- table(base$var1, base$var2)
write.table(tab, "work/directory/filename.xls", sep = ";")
write.csv2(tab, "work/directory/filename.csv")
# ou
copie(tab) # puis coller sur Excel
