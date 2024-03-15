library(tidyverse)

set.seed(2024) # aléa

ecrou <- tibble(
  per_id = str_pad(1:1000, 6, pad = "0"),
  an_deb = sample(2010:2015, 1000, replace = T),
  an_fin = sample(2016:2023, 1000, replace = T)
)

ecrou

# on veut les années entre an_deb et an_fin, avec seq() et une fonction de purrr !!!
# j'ai essayé tant de fois, le truc le plus proche était rowwise() mais sans map()
# et ça a pris 10000 ans

# inspo : https://www.r-bloggers.com/2018/09/using-purrrs-map-family-functions-in-dplyrmutate/#google_vignette

seq(2010, 2019)

(ecrou0 <- ecrou %>% 
  mutate(an_ecrou = map2(an_deb, an_fin, seq)) 
)
# an_ecrou au format liste, du coup c'est du unnest(), et pas la super fonction
# separate_wider/longer_delim
# cf decouvrir_fonctions/unnest & 3MW/20231115

# format long, une colonne avec toutes les années de présence, top
ecrou0 %>% 
  unnest_longer(an_ecrou) 

# format wide, autant de colonne que le nombre max d'années de présence
ecrou0 %>% 
  unnest_wider(an_ecrou, names_sep = "_") 
# ça fait très bien le travail, mais dans les cas de longues incarcérations,
# ça gonfle trop le nombre de colonne et je pense que mon PC n'appréciera pas.

# CCL => unnest_longer()
# c'est une manip que j'essaie de faire depuis juillet 2023, et hop je trouve
# la soluce en 4 petites lignes de code

# c'est à faire dans les cas où je dois faire des comptages de tête par an, mais 
# je dois enlever toutes les autres colonnes qui ne sont pas le nm_ecrou_init 
# et la colonne des années 

# petite visualisation
ecrou0 %>% 
  unnest_longer(an_ecrou) %>% 
  count(an_ecrou) %>% 
  ggplot(aes(x = an_ecrou, y = n)) +
  geom_point()
