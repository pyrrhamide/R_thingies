
library(tidyverse)
library(glue)

# All the housewives franchises ----
(rh <- tibble(
  franchise = c("BH", "NYC", "NJ", "SLC"),
  nf_season = c(13, 14, 13, 3)
))

# Real Housewives of Beverly Hills ----

## The cast ----
(rhobh_cast <- tibble(
  franchise = "BH",
  lname = c("Vanderpump","Richards","Grammer","Maloof","Armstrong",
            "Richards","Hadid","Rinna","Davidson","Girardi",
            "Kemsley","Mellencamp","Richards","Beauvais","Stracke",
            "Kung-Minkoff","Glanville","Gebbia","Giraud","Edwards",
            "Jenkins"),
  fname = c("Lisa","Kyle","Camille","Adrienne","Taylor",
            "Kim","Yolanda","Lisa","Eileen","Erika",
            "Dorit","Teddi","Denise","Garcelle","Sutton",
            "Crystal","Brandi","Carlton","Joyce","Kathryn",
            "Diana"),
  fullname = paste(fname, lname),
  f_season = c(1,1,1,1,1,
               1,3,5,5,6,
               7,8,9,10,11,
               11,3,4,4,7,
               12),
  l_season = c(9,13,2,3,3,
               5,6,12,7,13,
               13,10,10,13,13,
               13,5,4,4,7,
               12)
))

rhobh_cast |> arrange(f_season,l_season)

rhobh_cast <- rhobh_cast |> 
  rowwise() |> mutate(n_season = length(f_season:l_season)) |> ungroup() |> 
  mutate(map_dfc(
  1:13, \(x) ifelse(f_season <= x & l_season >= x, 1, 0)
) |>
  set_names(paste0("season_", 1:13)))

rhobh_cast |> arrange(desc(n_season))

### merci chat gpt
# for (season in 1:12) {
#   var_name <- paste0("season_", season)
#   
#   rhobh_cast <- rhobh_cast %>%
#     mutate(!!var_name := ifelse(season >= f_season & season <= l_season, 1, 0))
# }
# 
# rhobh_cast |> select(starts_with("season")) |> map_dbl(sum) 


# szn <- paste0("season_",1:12)
# rhobh_cast[,szn] <- NA

## The franchise & placements pendant l'intro ----
(rhobh_intro <- tibble(
  season = 1:12,
  diamond_mid = c(
    "camille grammer / kyle richards",
    "adrienne maloof / lisa vanderpump",
    "lisa vanderpump",
    "kyle richards",
    "kyle richards",
    "lisa vanderpump",
    "kyle richards / lisa vanderpump",
    "lisa vanderpump / kyle richards",
    "kyle richards",
    "kyle richards",
    "kyle richards",
    "kyle richards / dorit kemsley"
  ),
  diamond_left = c(
    "taylor armstrong",
    "kim richards",
    "taylor armstrong",
    "carlton gebbia",
    "kim richards",
    "kathryn edwards",
    "dorit kemsley",
    "dorit kemsley",
    "teddi mellencamp",
    "teddi mellencamp",
    "crystal kung-minkoff",
    "diana jenkins"
  ),
  diamond_right = c(
    "adrienne maloof",
    "taylor armstrong",
    "yolanda hadid",
    "kim richards",
    "eileen davidson",
    "erika girardi",
    "eileen davidson",
    "teddi mellencamp",
    "denise richards",
    "denise richards",
    "sutton stracke",
    "crystal kung-minkoff"
  ),
  intro_first = c(
    "taylor armstrong",
    "lisa vanderpump",
    "lisa vanderpump",
    "kyle richards",
    "lisa vanderpump",
    "lisa rinna",
    "lisa vanderpump",
    "kyle richards",
    "lisa rinna",
    "kyle richards",
    "kyle richards",
    "x"
  ),
  intro_last = c(
    "camille grammer",
    "camille grammer",
    "kyle richards",
    "lisa vanderpump",
    "kyle richards",
    "lisa vanderpump",
    "kyle richards",
    "lisa vanderpump",
    "kyle richards",
    "denise richards",
    "erika girardi",
    "x"
  )
))



season_numbers <- tibble(nb=1:5)

season_numbers |> 
  mutate(col_name = paste0("season_", nb)) |> 
  pivot_wider(names_from = col_name, values_from = nb)


df <- data.frame(matrix(nrow = 0,ncol = 5))

colnames(df) <- paste0("season_",1:5)
