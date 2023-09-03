
# packages ----
library(tidyverse)
library(data.table)
library(microbenchmark)
library(questionr)

# import ----

# import table des natinfs, on met les noms de variables en minuscule
  # direct en dplyr
n <- readxl::read_xlsx("natinf.xlsx") %>% 
  rename_with(~str_to_lower(.), everything())
  # en rbase
# n <- readxl::read_xlsx("natinf.xlsx") 
# names(n) <- tolower(names(n)) 

# faux fichier de natinf !
set.seed(2023) # nécessaire pour fonction faisant appel à l'aléa, garantie d'avoir les mêmes valeurs qu'importe la session

k <- tibble(
  natinf = 1:2000,
  blanchiment = sample(c("oui","non"),length(natinf),replace = TRUE,prob=c(.1,.9)),
  stup = sample(c("oui","non"),length(natinf),replace = TRUE),
  ffsd = sample(c("oui","non"),length(natinf),replace = TRUE),
  vesc = sample(c("oui","non"),length(natinf),replace = TRUE),
  teh = sample(c("oui","non"),length(natinf),replace = TRUE),
  probite = sample(c("oui","non"),length(natinf),replace = TRUE),
  recel = sample(c("oui","non"),length(natinf),replace = TRUE),
  njr = sample(c("oui","non"),length(natinf),replace = TRUE),
  fiter = sample(c("oui","non"),length(natinf),replace = TRUE)
)

sapply(k[-1],table)

object.size(k)
object.size(n)

# stat desc ----

# info table
class(n)
names(n)
dim(n)

n %>% map_dbl(n_distinct)
n %>% map_chr(class)

  # équivalent *apply
sapply(n,n_distinct)
sapply(n,class)

  # test "rapidité" dplyr v base
summary(microbenchmark(
  times = 20L,
  dplyr_nd = n |> map_dbl(n_distinct),
  base_nd  = sapply(n,n_distinct),
  dplyr_class = n |> map_chr(class),
  base_class  = sapply(n,class)
))
  # sapply(n,class) plus rapide. pour n_distinct, dplyr et base équivalents.
  # d'après docu de dplyr::n_distinct, fonction plus rapide que 
  # nrow(unique(data.frame(...)))

# nb natinf par type d'infraction sous-jacente
n %>% select(-natinf) %>% map(~count(data.frame(x = .x), x)) 
  # ou
n %>% select(-natinf) %>% map(table)
n |> select(-natinf) |> map_df(table)

  # rbase
sapply(n[-1],table) # lapply si on veut un tableau par colonne
sapply(n[-1],\(x) round(proportions(table(x)),5))
  # datatable
as.data.table(n)[,.N,by=stup] # hmmm, je vois pas comment faire tbh

summary(microbenchmark(
  times = 20L,
  dplyr_1 = n |>  select(-natinf) |> map(~count(data.frame(x = .x), x)),
  dplyr_2 = n |> select(-natinf) |> map(table),
  base = sapply(n[-1],table)
))
  # *apply encore plus rapide, suivi de dplyr_2

# nettoyage/recodage ----

# on change les chaines de caractères "oui/non" en binaire 0/1
n_rec <- n %>% 
  mutate(across(
    where(is.character),
    ~ case_when(. == "oui" ~ 1, TRUE ~ 0)))

n_rec %>% select(-natinf) %>% map(table)
n_rec |> select(-natinf) |> map_dbl(sum)
  # ou :)
n_rec %>% 
  summarise(across(stup:fiter,sum)) %>% 
  pivot_longer(everything(),names_to="contentieux",values_to="nb_inf")
  # majorité d'infractions de fraude fiscale, sociale ou douanière

# ou encore

n[1:5,]
head(
  data.frame(n[,1],
    sapply(n[,-1], \(x) ifelse(x=="oui",1,0))
    )
  ,5)

n_rec_b <- data.frame(
  n[,1], # colonne des natinfs
  sapply(n[,-1], \(x) ifelse(x=="oui",1,0)) # indicatrices groupes
)

lapply(n_rec_b[-1],table)
sapply(n_rec_b[-1],sum) # ...tellement moins bavard...

data.frame(nb_inf=sapply(n_rec_b[-1],sum))

# infractions qui appartiendraient à plusieurs groupes de sous-jacent?

# n_rec <- n_rec %>% 
#   rowwise() %>% mutate(n_sj = sum(c_across(stup:fiter))) %>% ungroup
  # très lent
# table(n_rec$n_sj)

n_rec |> 
  rowwise() |> 
  mutate(n_sj=sum(c_across(stup:fiter))) |> count(n_sj) # LENT!
# v
n_rec |> mutate(n_sj=rowSums(pick(-natinf))) |> count(n_sj)
# v
table(rowSums(n_rec[-1]))
n_rec[rowSums(n_rec[-1])==3,][1]

n_rec <- n_rec |> mutate(n_sj=rowSums(pick(-natinf)))

# quelle est cette natinf qui appartient à trois groupes?
n_rec[n_rec$n_sj==3,]
  # 20307: REALISATION D'UNE OPERATION FINANCIERE ENTRE LA FRANCE ET L'ETRANGER SUR DES FONDS PROVENANT D'INFRACTION A LA LEGISLATION SUR LES STUPEFIANTS : BLANCHIMENT DOUANIER
  # stup, ffsd, blanchiment

# combien d'infractions distinctes qui sont bien du blanchiment, et sous-jacent?
nrow(n_rec[n_rec$n_sj>0,]) # 1728

  # en data.table - clairement moins rapide
as.data.table(n_rec)[n_sj>0,.N]

# infractions BLANCHIMENT 
nrow(n_rec[n_rec$blanchiment==1,]) # 75 de blanchiment
nrow(n_rec[n_rec$blanchiment==1 & n_rec$n_sj==1,]) # 27 caractérisées comme QUE du blanchiment

# contentieux et uniquement ce contentieux
n_rec %>% 
  filter(n_sj==1) %>% 
  summarise(across(stup:fiter,sum)) %>% 
  pivot_longer(everything(),names_to="contentieux",values_to="nb_inf")

n_rec[n_rec$n_sj==1,-c(1,ncol(n_rec))] # c'est sport

sapply(n_rec[n_rec$n_sj==1,-c(1,ncol(n_rec))],sum)

summary(microbenchmark(
  times = 20L,
  dplyr = n_rec |>  
    filter(n_sj==1) |> 
    summarise(across(stup:fiter,sum)) |>  
    pivot_longer(everything(),names_to="contentieux",values_to="nb_inf"),
  base = sapply(n_rec[n_rec$n_sj==1,-c(1,ncol(n_rec))],sum)
))

###########################
# export table natinf csv #
###########################
write_csv2(n_rec,"natinf_lcbft.csv")
