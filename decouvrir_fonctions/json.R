## JSON ##

# toujours de R for Data Science (2ed)
# https://r4ds.hadley.nz/rectangling#json

pacman::p_load(repurrrsive, jsonlite, tibble, tidyr)

# Javascript Object Notation, is the way most web APIs return data.
# simple format designed to be easily read and written by machines  

# array [] like unnamed list e.g. => [null, 1, "string", false]
# object {} like a named list e.g. => {"x": 1, "y": 2}

# Note that JSON doesn’t have any native way to represent dates or date-times, so they’re often stored as strings, and you’ll need to use readr::parse_date() or readr::parse_datetime() to turn them into the correct data structure. Similarly, JSON’s rules for representing floating point numbers in JSON are a little imprecise, so you’ll also sometimes find numbers stored in strings. Apply readr::parse_double() as needed to get the correct variable type.

# 1. jsonlite ----

# to convert JSON into R data structures, jsonlite package.

# json files already loaded in repurrrsive
gh_users_json()

# read it with read_json()
gh_users2 <- read_json(gh_users_json()) # !!! parenthèses obligatoires de l'objet json !!!
gh_users2

# creating data with parse_json()
str(parse_json('1'))
str(parse_json('[1, 2, 3]')) # array 
str(parse_json('{"x" : [1, 2, 3]}')) # array within object

# 2. starting the rectangling process ----

# indeed, gh_users2 est crados. 
json <- '[
  {"name": "John", "age": 34},
  {"name": "Susan", "age": 27}
]'
json # toutes les infos sur une ligne, ou plus haut, une info par élément de la liste 

# tibble(json) pour que chaque élement devienne une ligne
df <- tibble(json = parse_json(json))
df

df |> unnest_wider(json) # "name" et "age" sont devenus les noms de colonnes ! 

# In rarer cases, the JSON file consists of a single top-level JSON object, representing one “thing”. In this case, you’ll need to kick off the rectangling process by wrapping it in a list, before you put it in a tibble.
json <- '{
  "status": "OK", 
  "results": [
    {"name": "John", "age": 34},
    {"name": "Susan", "age": 27}
 ]
}
'
json

tibble(json = parse_json(json)) # ah oui, dans une même colonne : un élement "simple" <chr [1]> et une liste à deux éléments <list [2]>

# soluce
df <- tibble(json = list(parse_json(json)))
df

df |> # named list
  unnest_wider(json) |> # list
  unnest_longer(results) |> # named list
  unnest_wider(results) # status = "OK" s'est dupliqué sur les deux lignes

# Alternatively, you can reach inside the parsed JSON and start with the bit that you actually care about:
df <- tibble(results = parse_json(json)$results) # on tej direct status
df

df |> 
  unnest_wider(results)

# 3. exercices (c l'école) ----

## Rectangle the df_col and df_row below. They represent the two ways of encoding a data frame in JSON.

json_col <- parse_json('
  {
    "x": ["a", "x", "z"],
    "y": [10, null, 3]
  }
')
json_row <- parse_json('
  [
    {"x": "a", "y": 10},
    {"x": "x", "y": null},
    {"x": "z", "y": 3}
  ]
')

json_col ; json_row

df_col <- tibble(json = list(json_col)) 
df_row <- tibble(json = json_row)

df_col |> unnest_wider(json) |> unnest_longer(c(x, y))
df_row |> unnest_wider(json)
