# Unnesting
# as seen on https://r4ds.hadley.nz/rectangling
# on commence à partir de 24.4 Case studies

pacman::p_load(purrr, repurrrsive)

# 1. Very wide data ----
# gh_repos: list that contains data about a collection of GitHub repos retrieved using the GitHub API. very deeply nested list.
View(gh_repos)

repos <- tibble(json = gh_repos)
repos
# "list" tout court : pas de nom donné aux éléments de la liste (26 ou 30 lignes dans ces dernières) => unnest_longer(), pour étaler sur une colonne.
(30*5)+26 # devrait y avoir 176 lignes

repos |> unnest_longer(json)
# "names list" : on a des noms ! 176 listes ! 68 lignes pour toutes les listes? => unnest_wider(), pour étaler sur plusieurs colonnes 
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json)
# beaucoup de colonnes + encore des sous-listes (owner). "we can see them all with names(); and here we look at the first 10"
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  names() |> head(10)

repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description)
# owner is another list column (avec des noms, donc on unnest_wider() :)
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner)
# ! sauf que dans owner, on a les mêmes noms de colonnes que id ! 
# la fonction est gentille, elle suggère d'utiliser `names_sep` ou `names_repair`
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner, names_sep = "_")
# pétard, 17 nouvelles variables. je sors du livre vite fait, je vais enlever les variables qui finissent par "_url" parce que yen a pas mal
repos |> 
  unnest_longer(json) |> 
  unnest_wider(json) |> 
  select(id, full_name, owner, description) |> 
  unnest_wider(owner, names_sep = "_") |> 
  select(!ends_with("_url")) # trop bien. je vois pas où ça s'appliquerai au taf, mais c'est cool

# 2. Relational data ----
# nested data is sometimes used to represent data that we'd usually spread across multiple data frames. for example, take got_chars which contains data about characters that appear in the GoT books and TV series. like gh_repos, it's a list, so we start by turning it into a list-column of a tibble
str(got_chars) # mauvaise idée. long
View(got_chars) # 30 lignes. 18 colonnes par ligne. 

chars <- tibble(json = got_chars)
chars # named list

chars |> unnest_wider(json) # titles, aliases, allegiances...en liste!
# sidetrack
chars |> unnest_wider(json) |> 
  select(name, playedBy) |> 
  unnest_longer(playedBy) |> 
  filter(playedBy != "") |> arrange(name) |> view()
  # intérêt des listes-colonnes : si même perso joué par plusieurs acteurs, au lieu de dédoubler la ligne caractère par nb d'acteurs, on laisse à une ligne et les acteurs sont dans une liste. smort!

# back to your main program 
chars |> unnest_wider(json) |> 
  select(id, name, gender, culture, born, died, alive)

chars |> unnest_wider(json) |> 
  select(id, name, where(is.list))
# let's explore the titles column. unnamed list => unnest_longer()
titles <- chars |> unnest_wider(json) |> 
  select(id, name, titles) |> 
  unnest_longer(titles) |> # j'ai un peu devancé Hadley Wickham :/ bref
  filter(titles != "") |> 
  rename(title = titles) # ahah Daenerys
titles

# sidetrack again: je veux essayer de tout mettre en un df, sans liste-colonne (je crois que c'était un des exos de Jenny Brian sur purrr)

## moment parfait pour enfin prendre au sérieux les tutos de JB ----
# https://jennybc.github.io/purrr-tutorial/ls01_map-name-position-shortcuts.html
got_list <- chars |> unnest_wider(json) |> select(id, name, where(is.list))
got_list

got_list |> unnest_longer(c(titles, aliases)) # peut pas faire plus d'un à la fois :(

got_list |> 
  unnest_longer(titles) |> 
  unnest_longer(aliases) |> 
  unnest_longer(allegiances) # ok, bon, je vais pas les faire un par un quand même. THINK!!!

got_list |> unnest_wider(c(books, povBooks, tvSeries), names_sep = "_") # ah

got_list |> map_if(is.list, \(x) unnest_longer(x)) # data must be a data frame, not a list, i don't get iiiiit

got_list |> 
  mutate(across(c(titles, aliases), unnest_longer)) # non

map_lgl(got_chars, "alive") |> set_names(map(got_chars, "name")) 

map(got_chars, `[`, c("name", "culture", "gender", "born"))
map_dfr(got_chars, magrittr::extract, c("name", "culture", "gender", "born"))
# list_rbind() fonctionne pas "each element of 'x' must be either a data frame or NULL. gah.
map(got_chars, `[`, c(3, 5, 4, 6))

# mouais. whatever

# 3. Deeply nested ----
# We’ll finish off these case studies with a list-column that’s very deeply nested and requires repeated rounds of unnest_wider() and unnest_longer() to unravel: gmaps_cities. This is a two column tibble containing five city names and the results of using Google’s geocoding API to determine their location:
gmaps_cities

gmaps_cities |> unnest_wider(json)
gmaps_cities |> unnest_wider(json) |> unnest_longer(results)
gmaps_cities |> unnest_wider(json) |> unnest_longer(results) |> unnest_wider(results) # pouah

locations <- gmaps_cities |> 
  unnest_wider(json) |> 
  select(-status) |> 
  unnest_longer(results) |> 
  unnest_wider(results)
locations
# 2 Washingtons, 2 Arlingtons

# determine the exact location of the match, stored in geometry list-col
locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> # bounds: rectangular region; location: a point
  unnest_wider(location)

locations |> 
  select(city, formatted_address, geometry) |> 
  unnest_wider(geometry) |> # bounds: rectangular region; location: a point
  select(!location:viewport) |> 
  unnest_wider(bounds) |> 
  rename(ne = northeast, sw = southwest) |> 
  unnest_wider(c(ne, sw), names_sep = "_")
# we unnest two columns at once by supplying a vector of variable names to unnest_wider()

# Once you’ve discovered the path to get to the components you’re interested in, you can extract them directly using another tidyr function, hoist():
locations |> 
  select(city, formatted_address, geometry) |> 
  hoist(
    geometry, 
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng")
  )

# for more adventures
vignette("rectangle", package = "tidyr")
