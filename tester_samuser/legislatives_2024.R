
# législatives anticipées 2024 - les candidats
# âges ? sexes ? âges des suppléants ? sexes des suppléants ? professions ?
# par département/circo/nuance ?
library(tidyverse)
library(janitor)

# lien stable liste candidats CSV
cand_leg_url <- "https://www.data.gouv.fr/fr/datasets/r/9efe7b76-8257-4db5-9e9f-37abb81ce65d"

import_candidats <- read_csv2(cand_leg_url) |> 
  clean_names()
  
candidats_leg_24 <- import_candidats |> 
  mutate(code_departement = if_else(str_length(code_departement) == 1,
                                    paste0("0", code_departement),
                                    code_departement)) |> 
  mutate(across(starts_with("date_"), dmy),
         across(starts_with("sortant"), \(x) if_else(is.na(x), F, T))) |> 
  mutate(age_titu = date_de_naissance_du_candidat %--% ymd(20240630) / years(1),
         age_supl = date_de_naissance_remplacant %--% ymd(20240630) / years(1))

# truc tout simple : sexe du titulaire v sexe du suppléant
candidats_leg_24 |> tabyl(sexe_du_candidat, sexe_remplacant)
# beaucoup d'échange hmm 

candidats_leg_24 |> tabyl(sexe_du_candidat) # 59%M
candidats_leg_24 |> tabyl(sexe_remplacant) # 56%M urgh
# sexe titulaire en fonction de la population/PIB/whatnot ? 

candidats_leg_24 |> 
  count(departement, sexe_du_candidat) |> 
  pivot_wider(names_from = sexe_du_candidat, values_from = n)

# femmes candidates par nuance politique 
candidats_leg_24 |> 
  count(code_nuance, sexe_du_candidat) |> 
  mutate(pct = n/sum(n), .by = code_nuance) |> 
  filter(sexe_du_candidat == "F") |> 
  arrange(pct) |> 
  print(n=Inf) # 48% des candidats du RN sont des candidates 

# 3eme circo de l'Ain et 4eme circo du VDM...les noms...
candidats_leg_24 |> 
  filter(code_circonscription %in% c("103", "9404")) |> 
  view()

# (les sortants tiens)
candidats_leg_24 |> tabyl(sortant) # attention l'interprèt : parmi tous les candidats, 13% sont des députés sortants 
# faudrait voir avec les candidats de 2022 qui est de nouveau candidat en 2024 
  
