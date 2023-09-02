#### Régressions sur R ####

## 1. packages à charger, nécessaire pour l'analyse ## ----
library(tidyverse)
library(questionr) # pour effectuer des tris à plat et des tris croisés
library(survey) # pour travailler avec des données pondérées
library(glm2) # pour effectuer régressions logistiques
library(nnet) # pour effectuer régressions logistiques polytomiques
library(GDAtools)


## 2. on importe les données ## ----


## 3. on observe les données - statistiques descriptives ## ----
dim(d)
names(d)
str(d)

sum(is.na(d))

summary(d$x)
unique(d$x)

table(d$x,d$y,useNA="ifany")
prop.table(table(d$x,d$y,useNA="ifany"))

# corrélation
cor(d$x,d$y,method='pearson')


## 4. on nettoie les données en prévision des régressions futures (notamment : on dichotomise les modalités des variables explicatives pour les régressions logistiques [codage disjonctif complet - Jérôme Deauvieau recommandait ça, mais on a établi que R arrivait à dichotomiser les modalités des variables comme un grand.]) ## ----

# exemple
# On a une variable couleur qui prend comme n modalités 1 = bleu ; 2 = blanc ; 3 = rouge. On va la transformer en n-1 variables (dans ce cas, 2) binaires, une indiquant si c'est bleu (0 = non ; 1 = oui), une indiquant si c'est blanc (idem). Il n'y a pas besoin de créer une troisième variable dichotomique car, par défaut, une observation qui est à bleu = 0 et blanc = 0 serait à rouge = 1 [nb. j'utilise tidyverse mais je te mets d'abord la méthode de JD]:
d$bleu[d$couleur==1] <- 1
d$bleu[d$couleur!=1] <- 0

d$blanc[d$couleur==2] <- 1
d$blanc[d$couleur!=2] <- 0

# moi je ferais
d <- d %>% mutate(bleu = ifelse(couleur==1,1,0),
                  blanc = ifelse(couleur==2,1,0))

# en gros, pour une variable à n modalités/catégories que tu veux utiliser dans une régression, tu génères n-1 variables dichotomiques. Toutefois, ce n'est pas du tout nécessaire parce que les fonctions de régressions de R dichotomisent automatiquement les modalités des variables catégorielles :)
# si tu as une variable à n modalités, tu peux aussi choisir de regrouper deux modas ensemble et de les coder en 0 et le reste des modas en 1.

# Important : une fois que tu as déterminé toutes tes variables explicatives, transforme les en facteur et change leur niveau. Les régressions fonctionnent avec des variables facteurs.
# exemple : une variable des mentions du bac qui est à l'origine sous la forme caractère. Les mentions seront rangées par ordre alphabétique et non par ordre hiérachique.
d$mentions <- factor(d$mentions, levels=c('Sans mention','Passable','Assez bien','Bien','Très bien'))


## 5. statistiques inférentielles ## ----

# La régression linéaire prend comme variable dépendante/à expliquer (var_dep) une variable quantitative continue, telle que la température, le PIB, mon envie de dormir progressive pendant les cours de Dhuot...les variables explicatives/indépendantes peuvent être quanti ou quali.
# Tu lis les coefficients de la régression de la manière suivante : "toutes choses égales par ailleurs, pour une unité de x (var_indep) en plus, y (var_dep) augmente/diminue de {coefficient}" ou bien "toutes choses égales par ailleurs, pour un changement de catégorie de x, y augmente/diminue de {coefficient}".

# régression linéraire simple (une seule var indep)
model_1 <- lm(var_dep ~ var_indep, data = d)
summary(model_1) # pour observer le modèle (coefficients, p.value [Pr(>|z|)], deviance, etc.)
# On interprète seulement les coefficients statistiquement significatifs (avec des petites étoiles). Toutefois ça peut être intéressant de dire "je suis surprise que cette variable n'est pas d'effet, ce qui va à l'encontre de mon hypothèse/de la littérature, blabla."

# régression linéraire multiple (plusieurs var indeps) sans intéraction
model_2 <- lm(var_dep ~ var_indep1 + var_indep2 + var_indep3, data = d)
# si on souhaite obtenir un modèle pondéré
model_3 <- lm(var_dep ~ var_indep1 + var_indep2 + var_indep3, data = d, weights = poids)
# on peut aussi "mettre à jour" le modèle 2
model_3 <- update(model_2, . ~ ., weights=poids)

# régression linéaire multiple avec intéraction
model_4 <- lm(var_dep ~ var_indep1 + var_indep2 + var_indep3 + var_indep1*var_indep2, data = d, weights = poids)
# ou
model_4 <- update(model_3, . ~ . + var_indep1*var_indep2)
# le coef d'intéraction s'additionne aux coefs de base.

# Cependant en sociologie, il est rare d'avoir une variable continue à expliquer. On a généralement des variables catégorielles (qui peuvent être une variable quanti transformée en variable quali). Le modèle de régression linéaire ne fonctionne plus dans ce cas, on se tourne vers le modèle de régression linéaire de probabilité qui prend comme variable dépendante/à expliquer une variable dichotomique (0/1).
# La ligne R reste la même, seule la nature de la var_dep change.
# Tu lis le coefficient de cette manière : "toutes choses égales par ailleurs, pour une unité de plus/pour un changement de catégorie de x, la probabilité que l'évènement y=1 se passe augmente/diminue [je suis pas sûre de la lecture exacte du coefficient, mais en gros ça fonctionne comme ça.]"

# régression linéaire de probabilité
model_5 <- lm(var_dep_dicho ~ var_indep1 + var_indep2 + var_indep3, data = d, weights = poids)

# Pour une raison qui m'échappe parce que je suis pas très fraîche et parce que je sais pas (faudra demander à Ricardo), on n'utilise pas le modèle de régression linéaire de probabilité mais plutôt le modèle de régression logistique dichotomique. Pour les coefficients, on parlera de l'effet du logit de probabilité que l'évènement y=1 se passe.
# On commence par lancer le modèle de régression logistique à effets principaux, ou modèle de l'indépendance :
model_6 <- glm(var_dep_dicho ~ var_indep1 + var_indep2 + var_indep3, data = d, weights = poids, family=binomial)

# Petite note sur le modèle de l'indépendance totale : c'est le modèle dans lequel tu balances tes variables explicatives sans intéraction/association, pour vérifier si les variables sont indépendantes entre elles, càd si [toutes choses égales par ailleurs ^^] elles expliquent le phénomène à elles seules. Cependant, en socio, c'est inimaginable que la classe sociale et le sexe agissent indépendamment l'un de l'autre, c'est pour cette raison qu'on choisirait d'intéragir ces variables entre elles.
# Pour vérifier si les variables sont indépendantes, tu regardes la residual deviance : si c'est un nb énorme, direction intéraction-ville.

# Passons à un truc dont on parle beaucoup en classe : les odds ratio. Les coefficients d'une régression logistique ne sont pas très évidents à interpréter (dans le sens où c'est long de dire "le logit de probabilité"). On passe donc les coefficients logit par la fonction exponentielle, ce qui nous donne les odds ratio (OR). Un OR est la chance qu'un évènement y=1 se passe pour une condition B, par rapport à ce que cet évènement y=1 se passe pour une condition A (condition de ref). L'OR est compris entre 0 et +infini. Pour un OR compris entre 0 et 1, la chance que le truc se passe pour un groupe B est en fait moindre que la chance qu'il se passe pour le groupe A, mais on dira quand même "l'évènement a 0.14 fois plus de chance de se passer pour le groupe B que pour le groupe A".
# Pour obtenir les coefficients de la régression en OR, on utilise la commande suivante :
exp(model_7$coefficients)

# On met le modèle précédent à jour, en inclant des intéractions :
model_7 <- update(model_6, . ~ . + var_indep1*var_indep2) # pour une unique intéraction
model_8 <- glm(var_dep_dicho ~ (var_indep1 + var_indep2 + var_indep3)^2, data = d, weights = poids, family=binomial) # pour intéragir toutes les modalités de toutes les variables entre elles (intéraction d'ordre 2)
model_9 <- glm(var_dep_dicho ~ (var_indep1 + var_indep2)*var_indep3, data = d, weights = poids, family=binomial) # pour intéragir var_indep3 avec var_indep1 et var_indep2 [plus rapide que d'écrire individuellement chaque intéraction]

# On veut voir si l'ajout de ces intéractions est statistiquement significative (c'est la méthode de Deauvieau, Parodi a la sienne que je détaille plus bas). On effectue un test statistique sur l'intéraction, qui est un test du khi-deux sur la différence de vraisemblance entre les modèles d'indépendance et d'intéraction.
test <- 2*(logLik(model_7)-logLik(model_6)) [1]
# Entre ces deux modèles, il n'y a qu'une variable/modalité en plus, donc le degrée de liberté est de 1 (je sais plus pourquoi).
1-pchisq(test,1) # (test,1), 1 étant le ddl.

# On a vu la régression logistique dichotomique. Mais pour une variable à expliquer qui contient plus de 2 modalités, on utilise le modèle de régression logistique polytomique.
# Imaginons que tu as une variable n à expliquer avec des modalités 1 (modalité de référence), 2 et 3, et une variable m explicative avec des modalités A (modalité de ref), B et C. Tu interpréteras un coefficient ainsi : "toutes choses égales par ailleurs, le logit de probabilité que l'évènement y=2 se produise, par rapport à l'évènement y=1, pour le groupe B augmente/diminue par rapport au groupe A".
# C'est une phrase assez dégueulasse. Toutefois c'est un modèle de régression sur lequel on ne s'est pas attardés l'année dernière donc je n'en dis pas plus. Voici la commande pour lancer le modèle.
model_10 <- multinom(var_dep_poly ~ var_indep1 + var_indep2 + var_indep3, data=d, weights=poids)


## Cours de Maxime Parodi - la régression log-linéaire ## ----

# Résumons les différents modèles de régression qu'on a vu jusqu'à maintenant :
# - régression linéaire simple/multiple : variable à expliquer quantitative, toutes les variables sont dans leurs unités de bases.
# - régression linéaire de probabilité : variable à expliquer dichotomique, toutes les variables sont dans leurs unités de bases.
# - régression logistique dichotomique/polytomique : variable à expliquer dichotomique, toutes les variables sont mises à l'échelle logarithmique (log(x)).
# On passe maintenant à la régression log-linéaire, que je résumerai (techniquement) ainsi : la variable à expliquer est mise à l'échelle logarithmique, les variables explicatives restent dans leurs unités de bases. Ce modèle est pratique pour expliquer une évolution exponentielle, par exemple l'évolution des cas de coronavirus qui est lente au début, augmente très rapidement dans un interval de temps court, puis plafonne.
# Je t'avoue que j'ai pas vraiment compris/écouté son cours parfaitement, mais il y a quelque chose avec des tableaux croisés à trois dimensions (j'utiliserai l'exemple des admissions à Berkeley : admissions*département*sexe).

# Les packages et fonctions
library(vcd)
library(DescTools)
# source("f.util.cours.R") fonction créée par MP que j'inclus direct ici
stat_ajust <- function(...) {
  list_glm <- enquos(...)
  noms <- as.character(list_glm) %>% map_chr(~str_sub(.x, start = 2))
  list_glm <- map(list_glm, rlang::eval_tidy)

  return(map2_dfr(list_glm, noms, ~ tibble(
    model = .y,
    G2 = .x$deviance,
    ddl = .x$df.residual,
    p.value.G2 = 1 - pchisq(.x$deviance, .x$df.residual),
    dissimilarity = sum(abs(.x$y - .x$fitted.values)) / sum(.x$y) / 2,
    AIC = .x$aic,
    BIC = AIC(.x, k = log(sum(.x$y)))
    )))
}

# La base
Berkeley <- UCBAdmissions %>% as.data.frame()

# Modèle log-linéaire
# Modèle de l'indépendance totale
M0 <- glm(Freq ~ Gender + Admit + Dept, family = poisson, data = UCBAdmissions)
# La différence avec la reg logit est la famille de la distribution. Pour la reg logit, on avait family=binomial, ici on a family=poisson.
summary(M0)
# Residual deviance = 2097.7 on 16 degrees of freedom.
# Il reste beaucoup trop d'information à expliquer que le modèle de l'indépendance totale n'explique pas, trop pour établir l'indépendance entre les variables. On décide alors de mettre à jour le modèle en ajoutant des intéractions.

# une interaction prise en compte
M1_GD <- update(M0, . ~ . + Gender:Dept) # choix du département est genré
M1_GA <- update(M0, . ~ . + Gender:Admit) # admission discriminante en fonction du sexe
M1_AD <- update(M0, . ~ . + Admit:Dept) # départements plus ou moins selectifs

# deux interactions
M2_GD.GA <- update(M1_GD, . ~ . + Admit:Gender)
M2_GD.AD <- update(M1_GD, . ~ . + Admit:Dept)
M2_AD.GA <- update(M1_AD, . ~ . + Admit:Gender)

# trois interactions d'ordre 2
M3 <- update(M2_GD.AD, . ~ . + Gender:Admit)

# une interaction d'ordre 3 = modèle saturé
M4 <- update(M0, . ~ . + Gender*Admit*Dept) # sélectivité du département varie selon le sexe

# Tous les modèles sont stockés dans la mémoire de R, on veut maintenant voir le gain ou la perte d'information offert.e par chaque nouveau modèle.
anova(M0, M1_GA, M1_AD, M1_GD, M2_AD.GA, M2_GD.GA, M2_GD.AD, M3, M4)
# En regardant la dernière colonne, 'Deviance', on peut voir que les modèles 3 (les départements sont sélectifs) et 7 (les départements sont sélectifs et genrés) sont ceux grâce auxquels on a gagné le plus d'information. Toutefois entre le modèle 3 et le modèle 7, ce dernier a moins de residual deviance, c'est donc le modèle qui expliquerait le mieux la variance des données. Visiblement, la sélectivité des départements et leurs compositions sont des éléments importants à inclure dans la régression.
# Pourquoi ne pas prendre le modèle 9 ? Parce que le modèle 9 est saturé (toutes les combinaisons d'intéractions sont dans le modèle, c'est logique que toutes les données soient expliquées.)
# Revenons en au modèle 7. Comment peut-on s'assurer que le gain d'info est réel et pas dû au hasard (n'est pas du bruit)?
stat_ajust(M0, M1_GA, M1_AD, M1_GD, M2_AD.GA, M2_GD.GA, M2_GD.AD, M3, M4)
# (1) Regardons la colonne 'p.value.G2' :une p.value de 0 est suspicieux, on ne prend pas en compte les modèles qui ont cette valeur ;
# (2) 'dissimilarity' : proportion des observations 'mal classées'. Le plus bas, le mieux. De ce fait, le modèle 7 est toujours le meilleur, ce qui est confirmé par son BIC qui est le plus faible de tous les modèles (jsp ce que ça mesure par contre.)
