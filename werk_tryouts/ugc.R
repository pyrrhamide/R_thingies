
# packages
pacman::p_load(data.table)

# aléa
set.seed(14)

# vecteurs pour construire table UGC
les_ugc <- c("Normale", "Arrivant", "Semi-liberté", "QCP", "PMR", "SAS")
les_quartiers <- c("QMA", "QMC", "QCD", "QSL", "SAS", "QPA")
les_etab <- c("MA", "MC", "CD", "CSL", "EPM")
les_disp <- c("bordeaux", "dijon", "lille", "lyon", "paris", 
              "marseille", "rennes", "strasbourg", "toulouse", "dspom")

# tables avec probas
ugc <- data.table(
  nat_ugc = les_ugc, proba = c(0.4, 0.2, 0.2, 0.02, 0.05, 0.13)
)

disp <- data.table(
  disp = les_disp, proba = c(0.1, 0.05, 0.1, 0.125, 0.15, 
                             0.125, 0.05, 0.1, 0.15, 0.05)
)

# table etab
etab <- data.table(
  etab = c(les_etab, rep("CP", length(les_quartiers))),
  quartiers = c(les_etab, les_quartiers),
  eff = c(rep(100,5), rep(40, 6))
)

# reprex table de la requête :)
t <- data.table(
  type_etab = rep(etab$etab, etab$eff),
  lc_etab = paste(rep(etab$etab, etab$eff), 
                  rep(paste0(LETTERS, LETTERS), times = sum(etab$eff))),
  disp = paste("disp", sample(disp$disp, sum(etab$eff), 
                              replace = T, prob = disp$proba)),
  quartier = rep(etab$quartiers, etab$eff),
  nature_ugc = sample(ugc$nat_ugc, sum(etab$eff), 
                      replace = T, prob = ugc$proba),
  places_hf = sample(c("H", "F", "I"), sum(etab$eff), 
                     replace = T, prob = c(0.6, 0.2, 0.2)),
  places_min = sample(c("O", "N", "I"), sum(etab$eff),
                      replace = T, prob = c(0.3, 0.6, 0.1)),
  places_sl = sample(c("O", "N", "I"), sum(etab$eff),
                     replace = T, prob = c(0.25, 0.70, 0.05)),
  places_pmr = sample(c("O", "N", "I"), sum(etab$eff),
                      replace = T, prob = c(0.05, 0.90, 0.05)),
  cap_oper = sample(1:3, sum(etab$eff), replace = T, prob = c(0.7, 0.2, 0.1))
)

# explo de base, de la table "normale"
str(t)

t[, .N, nature_ugc]
t[, sum(cap_oper), nature_ugc]
t[, sum(cap_oper), quartier]

dcast(t, places_hf + places_min ~ nature_ugc, value.var = cap_oper, fun = sum)

t[, sum(cap_oper), .(quartier, nature_ugc)]

t[, .N, .(places_hf, places_min)]
t[, sum(cap_oper), .(places_sl)]

# recodage
t[, .(
  sexe_strict = if_else(places_hf == "F", "F", "H"),
  disc_strict = if_else(places_min == "O", "min", "maj"),
  nat_ugc_rec = case_when(
    places_sl == "O" & nature_ugc == "Normale" ~ "Semi-liberté",
    places_pmr == "O" & nature_ugc == "Normale" ~ "PMR"
  )
)]
