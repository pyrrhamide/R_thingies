
### nettoyer des tableaux extraits du world wide web (wikipédia) ###

# très intéressant : parse_number() au lieu de as.numeric() que je fais toujours, pour transformer du texte en numérique

# packages
library(tidyverse)
library(rvest)
library(janitor)

# lien 
wiki_url <- 'https://en.wikipedia.org/wiki/Taylor_Swift_albums_discography'

# on récupère tous les tableaux de la page wiki 
all_tables <- read_html(wiki_url) |> 
  html_table()

all_tables

# on prend le deuxième
tableau <- all_tables[[2]]
tableau # les noms de colonnes sont sur la première ligne 

# on nettoie les noms de colonnes
moved_row_tib <- tableau |> 
  row_to_names(row_number = 1) |> 
  clean_names() |> 
  # on enlève la dernière ligne, inutile
  slice_head(n = -1) |> 
  select(-starts_with("na"))

moved_row_tib

# clean up album details (release date, label and formats) ----
moved_row_tib |> 
  select(title, album_details)
# info sur 3 colonnes : Released, Label, et Formats

release_label_formats <- moved_row_tib |> 
  select(title, album_details) |> 
  separate_wider_delim(
    cols = album_details,
    delim = "\n",
    names = c("released", "label", "formats")
  ) |> 
  mutate(across(released:formats, \(x) str_extract(x, "(?<=: ).*"))) |> 
  mutate(released = mdy(released)) |> 
  separate_wider_delim(
    cols = formats,
    delim = ", ",
    names_sep = "_",
    too_few = "align_start"
  )

release_label_formats # à ma sauce mais bon, ça fait le taf

release_label_formats |> 
  pivot_longer(starts_with("formats"),
               values_to = "formats",
               values_drop_na = TRUE) |> 
  select(-name)

# PEAK (/\) positions ----
peak_positions <- moved_row_tib |> 
  select(title:uk_15, -album_details) |> 
  pivot_longer(
    cols = -1,
    names_to = 'country',
    names_pattern = '(.+)_.+', # neat! on peut aussi mettre * à la place de +
    values_to = 'peak_position'
  ) |> 
  mutate(peak_position = parse_number(peak_position, na = c("—", "TBA")))
# au lieu de mutate(peak_position = as.numeric(peak_position))!!

peak_positions

peak_positions |> 
  ggplot(aes(x = country, y = peak_position, colour = title)) +
  geom_point()

# extracting the sales ----
moved_row_tib |> select(title, sales_a)

# je freetyle ! sinon voir 3MW du 22/11/2023
## avec separate_wider_delim
moved_row_tib |> 
  select(title, sales_a) |> 
  # un peu pareil que pour détails albums
  separate_wider_delim(cols = sales_a, delim = "\n", names_sep = "",
                       too_few = "align_start") |> 
  pivot_longer(starts_with("sales_"), 
               values_drop_na = T) |> 
  separate_wider_delim(cols = value, delim = ": ", names = c('country', 'sales'),
                       too_few = "align_end") |> 
  mutate(sales = str_remove_all(sales, "\\[|\\]|[A-Z]") |> parse_number())

## avec separate_longer_delim -- beaucoup plus concis!
moved_row_tib |> 
  select(title, sales_a) |> 
  separate_longer_delim(cols = sales_a, delim = "\n") |> 
  separate_wider_delim(cols = sales_a, delim = ": ", names = c('country', 'sales'),
                       too_few = "align_start") |> 
  mutate(sales = str_remove_all(sales, "\\[|\\]|[A-Z]") |> parse_number())

# ah oui.
# il créé un vecteur de tous les pays distincts, qu'il collapse pour en faire un regex, il y a du unnest(), pour finir sur un separate_wider_delim()...

# certifications (3MW du 22/11/2023) ----
moved_row_tib |> 
  select(title, certifications)

## my way ----
moved_row_tib |> 
  select(title, certifications) |> 
  separate_longer_delim(cols = certifications, delim = "\n") |> 
  separate_wider_delim(cols = certifications, delim = ": ", 
                       names = c("association", "certification"), 
                       too_few = "align_start") |> 
  # complexe, parce que je veux enlever les chiffres à la fin 
  # mutate(amount = str_remove_all(amount, "\\[|\\]|[0-9]$")) |> 
  mutate(certification = str_remove_all(certification, "\\[(.*)\\]$")) |>  # omg j'ai trouvé le regex seule
  # il ajoute un 1x quand c'est pas spécifié, let's go
  # mutate(certification = if_else(!grepl("[0-9]", certification), 
  #                                paste("1x", certification), certification)) |> 
  # ah, il fait ça pour ensuite faire une colonne avec le nombre de fois 
  # du coup pas besoin de l'étape du dessus 
  mutate(number = coalesce(parse_number(certification), 1),
         certification = str_remove_all(certification, ".+ ")) # pattern d'AR cette fois (removes everything after white space) ("^[0-9]+× " ??? ça fonctionnait pas parce que c'est un × ET NON UN x que suis-je bête)

## his way ----
# extracting the certification names
certifications_regex <- moved_row_tib$certifications |> 
  str_extract_all("[A-Z]+:") |> # ah, donc le + est litéral???
  unlist() |> 
  unique() |>  # hmmm pas bon pour IFPI, décliné en DEN et NOR
  paste0(collapse = "|")

# extracting the amount of platinum, gold, etc. labels
certifications_numbers_regex <- glue::glue("({certifications_regex}) ([0-9]+× )?[a-zA-Z]+")
# × et non x !!!

certifications_numbers_regex

# using it in str_extract_all and unnesting (il se complique la vie, il utilisait separate_wider_delim mais pas separate_longer_delim??)
unnested_and_split_certifications <- moved_row_tib |> 
  select(title, certifications) |> 
  mutate(certifications = str_extract_all(certifications, 
                                          certifications_numbers_regex)) |> 
  unnest(cols = certifications) |> 
  separate_wider_delim(cols = certifications, delim = ": ",
                       names = c('association', 'certification'))

unnested_and_split_certifications

# on ajoute le 1× (et. non. x.)
unnested_and_split_certifications |> 
  mutate(certification = if_else(str_detect(certification, '[0-9]+'),
                                 certification, paste("1×", certification))) |> 
  # extracting them numeros
  mutate(number = parse_number(certification),
         certification = str_remove_all(certification, ".+ "))

# un peu beaucoup 
