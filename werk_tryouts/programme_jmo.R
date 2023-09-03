
# packages ----
pacman::p_load(tidyverse, data.table, microbenchmark)

# paramètres ----
an <- 2022

andeb <- dmy(paste(1,1,an))
anfin <- dmy(paste(31,12,an))

jours <- seq(andeb, anfin, 1)
jours

# aléa ----
set.seed(1998)

# fausse table :) ----
vec_mes <- c("SME", "SPROB", "ARSE", "723-15", "CJ", "SSJ", "PSEM", "DDSE",
             "AP", "STA80", "STA85", "SUSPM")

info <- tibble(
  a = c("ROMEO", "JULIETTE", "TYBALT", "MERCUTIO", "BENVOLIO",
        "CAPULET", "MONTAIGU"),
  b = c(50, 90, 20, 60, 80, 150, 120),
  c = b/10L
)

t <- tibble(
  spip = map2(info$a, info$b, ~ rep(str_glue("SPIP {.x}"), .y)) |> list_c(),
  code_spip = map(info$b, 
                  \(x) rep(round(runif(1, min = 3e4, max = 5e4), 0), 
                           each = x)) |> unlist(),
  aff_id = round(runif(length(spip), min = 10e4, max = 90e4), 0),
  per_id = map(info$c, 
                   \(x) rep(round(runif(10, min = 10e4, max = 90e4), 0), 
                            each = x)) |> unlist(),
  pec_id = round(runif(length(spip), min = 10e4, max = 90e4), 0), # pas certaine de l'individualisation du pec_id
  mes = sample(vec_mes, length(spip), replace = TRUE),
  dt_deb = sample(jours[1:120], length(spip), replace = TRUE),
  dt_fin = sample(jours[200:length(jours)], length(spip), replace = TRUE)
)

glimpse(t)

# nombre de jours suivis pour personnes avec plusieurs mesures ----
date_test <- sample(jours, 1)

## DPLYR ----
t_int <- t |> 
  mutate(interval = interval(dt_deb, dt_fin)) |> 
  filter(date_test %within% interval) |> 
  mutate(ident = str_c(code_spip, per_id), .after = pec_id) |> 
  mutate(rang = row_number(), .by = ident, .after = ident)

t_int |> 
  group_by(code_spip, spip, rang) |> 
  summarise(n_per = n_distinct(pec_id)) |> 
  pivot_wider(names_from = rang, values_from = n_per, values_fill = 0)

## R BASE (avec quelques fonctions dplyr et data.table) ----
t_base <- t

subset(t_base, dt_deb < date_test & date_test < dt_fin)
# ou 
subset(t_base, between(date_test, dt_deb, dt_fin)) # différence d'une colonne! mêmes lignes qu'en dplyr, j'en mets ma main à couper

subset(t_base, between(date_test, dt_deb, dt_fin, incbounds = FALSE)) # et voila. parce que exclusion des bornes. discuter avec SM (et FDB?) sur quoi choisir.

t_base <- subset(t_base, between(date_test, dt_deb, dt_fin))
t_base$ident <- str_c(t_base$code_spip, t_base$per_id)
t_base$rang <- rsu

frank(t_base, ident, ties.method = "min")
with(t_base, by(t_base, ident, \(x) frank(x)))


## DATA TABLE ----
t_dt <- as.data.table(t) 
  # setDT(t) transforme par assignation directe, pas besoin de `<-`

t_dt_int <- t_dt[between(date_test, dt_deb, dt_fin), 
                 ][, ident := str_c(code_spip, per_id)
                   ][, rang := rowid(ident, prefix = "mes_")]

haine <- t_dt_int[, .(n = uniqueN(pec_id)), by = .(code_spip, spip, rang)]

dcast(haine, ... ~ rang, value.var = "n", fill = 0)

### apparenté : les jours totaux de suivis par type de mesure ----
#### en faisant la somme dans le dt agrégé
bleep <- t_dt[
  , .(code_spip, spip, mes, ecart = dt_deb %--% dt_fin / days(1))
  ][, sum(ecart), by = .(code_spip, spip, mes)]

dcast(bleep, ... ~ mes, fill = 0)

#### en faisant la somme dans le dcast (fun.aggregate = sum). dt individuel
bleep2 <- t_dt[, .(spip, code_spip, mes, ecart = dt_deb %--% dt_fin / days(1))]
  # keyby ordonne les lignes en fonction des variables indiquées (by + arrange)
  # quoique mettre les variables dans j semble ordonner aussi...

dcast(bleep2, ... ~ mes, fun = sum, fill = 0) # trop bieng

# en fonctions - calcul temps d'exécution ----

## dplyr ----
mes_dplyr <- function(ma_date){
  t_int <- t |> 
    mutate(interval = interval(dt_deb, dt_fin)) |> 
    filter({{ma_date}} %within% interval) |> 
    mutate(ident = str_c(code_spip, per_id), .after = pec_id) |> 
    mutate(rang = row_number(), .by = ident, .after = ident)
  
  t_int |> 
    summarise(n_per = n_distinct(pec_id), .by = c(code_spip, spip, rang)) |> 
    pivot_wider(names_from = rang, values_from = n_per, values_fill = 0)
}

mes_dplyr(ymd("2022-02-14"))

map(jours[34:40], mes_dplyr) |> 
  list_rbind() |> 
  group_by(code_spip, spip) |> 
  summarise(across(where(is.integer), \(verone) sum(verone, na.rm = TRUE)))

## datatable ----
mes_dt <- function(ma_date){
  t_dt_int <- t_dt[between(ma_date, dt_deb, dt_fin), 
    ][, ident := str_c(code_spip, per_id)
    ][, rang := rowid(ident, prefix = "mes_")]
  
  haine <- t_dt_int[, .(n = uniqueN(pec_id)), by = .(code_spip, spip, rang)]
  
  dcast(haine, ... ~ rang, value.var = "n", fill = 0) |> as_tibble()
}

mes_dt("2022-02-14")

microbenchmark(
  map(jours[34:40], mes_dt) |>
    list_rbind() |> 
    group_by(code_spip, spip) |> 
    summarise(across(where(is.integer), \(verone) sum(verone, na.rm = TRUE)))
  ,
  
  as.data.table(
    list_c(lapply(jours[34:40], mes_dt))
  )[, lapply(.SD, sum), by = .(code_spip, spip), .SDcols = is.integer]
)


## benchmarking! ----

microbenchmark(
  
  map(jours, mes_dplyr) |> 
    list_rbind() |> 
    group_by(code_spip, spip) |> 
    summarise(across(where(is.integer), \(verone) sum(verone, na.rm = TRUE))),
  
  map(jours, mes_dt)|> 
    list_rbind() |> 
    group_by(code_spip, spip) |> 
    summarise(across(where(is.integer), \(verone) sum(verone, na.rm = TRUE)))
)

# mes_dt clairement plus rapide (100 évals). 
# [dplyr] 9.01 sec v [dt] 2.05 sec, min
# [dplyr] 13.38 sec v [dt] 3.07 sec, moyenne
# [dplyr] 11.66 sec v [dt] 2.66 sec, médiane
# [dplyr] 32.48 sec v [dt] 7.24 sec, max


