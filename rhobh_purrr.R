library(tidyverse)
library(glue)

(rh <- tibble(
  franchise = c("bh","atlanta","ny","slc","nj"),
  n_fr_season = c(13,14,14,4,11)
))
  
# Beverly Hills ----
bh <- tibble(
  franchise = "bh",
  fname = c("kyle","lisa","yolanda","joyce","lisa","garcelle"),
  lname = c("richards","vanderpump","foster","giraud","rinna","beauvais"),
  fullname = paste(fname, lname),
  first = c(1,1,3,4,5,10),
  last = c(13,9,5,4,12,13)
) %>% 
  rowwise() %>% 
  mutate(n_h_season = length(seq(first,last))) %>% ungroup

bh

rhobh <- bh %>% left_join(rh) %>% select(franchise, n_fr_season, everything())

# housewife présente dans la saison 10 ?
ifelse(rhobh$first <= 10 & rhobh$last >= 10, TRUE, FALSE)

# test pour nom saison 
map(1:13,\(x) glue("season_{x}"))

# housewife présente dans saisons x à y - avec info first & last season
map_dfc(1:13, \(x) ifelse(rhobh$first <= x & rhobh$last >= x, TRUE, FALSE)) %>% 
  set_names(paste0("season_",1:13))

# housewife présente dans saisons x à y - avec info nb saisons franchise
sos <- rhobh %>% mutate(
  map_dfc(
    seq_len(n_fr_season), \(x) ifelse(rhobh$first <= x & rhobh$last >= x, 1, 0)) %>% 
    set_names(paste0("season_",seq_len(n_fr_season)))
)

# version finale rhobh!
ttt <- rhobh %>% mutate(
  map_dfc(1:13, \(x) ifelse(rhobh$first <= x & rhobh$last >= x, 1, 0)) %>% 
  set_names(paste0("season_",1:13))
  )


# Salt Lake City ----
rho_combine <- tibble(
  franchise = "slc",
  fname = c("lisa","meredith","jen","whitney","heather","mary"),
  lname = c("barlow","marks","shah","rose","gay","cosby"),
  fullname = paste(fname, lname),
  first = c(1,1,1,1,1,1),
  last = c(4,4,3,4,4,2)
) %>% 
  rowwise() %>% 
  mutate(n_h_season = length(seq(first,last))) %>% 
  ungroup %>% 
  bind_rows(bh) %>% 
  left_join(rh) %>% select(franchise, n_fr_season, everything())

rho_combine <- rho_combine %>% mutate(
  map_dfc(
    seq_len(max(n_fr_season)), \(x) ifelse(rho_combine$first <= x & rho_combine$last >= x, 1, 0)) %>% 
    set_names(paste0("season_",seq_len(max(n_fr_season))))
)


# New Jersey ----
nj <- tibble(
  franchise = "nj",
  fname = c("Teresa","Dina","Caroline","Danielle","Jacqueline"),
  lname = c("Giudice","Manzo","Manzo","Staub","Laurita"),
  fullname = paste(fname, lname),
  first = c(1,1,1,1,1),
  last = c(11,2,5,2,7),
) 
