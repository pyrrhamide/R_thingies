
# https://alberts-newsletter.beehiiv.com/p/clean-time-data

# packages 
library(tidyverse)
library(lubridate)

# données 
toy_data <- tibble(
  dates = c(
    'June 21, 2023', 'April 25, 2023', 'May 11, 2023', '06-05-2023'
  )
)

toy_data
# format caractère, no bueno. on peut pas faire des opérations comme ça, sans préciser à R que ce sont des dates. (ie year(dates) ne fonctionnera pas parce que c'est pas au format date, regardons ça tout de suite)
year(toy_data$dates)

# soluce 1: reader::parse_date() ----
toy_data |> mutate(dates = parse_date(dates, format = "%B %d, %Y"))
# sauf que la dernière observation est pas formattée comme les autres, donc erreur. mais on va pas faire une ligne par format comme même...

# soluce 2: lubridate::parse_date_time() ----
toy_data |> mutate(dates_parsed = parse_date_time(dates, orders = 'mdy'))
# instead of using a format argument, this function uses an orders argument where we can specify the time formats without the % abbreviations. Here, mdy is an abbrev for "month, day, year".
# no need to specify other caracters like , or - in order to get a proper translation. parse_date_time() FTW!
# sauf que, imagine tu reçois un fichier Excel crados, où les dates sont rentrées d'une manière diff, dans un ordre diff par observation. faut que je regarde si la fonction fonctionne.

data_crade <- tibble(
  date = c("14 novembre 1998", "février 1970", "1969-04-24", "27/7/2002")
)
data_crade |> mutate(date_clean = parse_date_time(date, orders = 'dmy')) # as expected 

# parse multiple formats all at once (il a lu mes pensées le boug) ----

# what if the last date in our data set actually means May 6th rather than June 5th? in that case the last date would use dmy instead of mdy like the rest.
# wellz, lubridate::parse_date_time() can parse multiple formats all at once. you just have to fill orders with a vector containing multiple formats in the right order.
toy_data |> 
  mutate(dates_parsed = parse_date_time(dates, orders = c('dmy', 'mdy')),
         dates_parsed2 = parse_date_time(dates, orders = c('mdy')))

data_crade |> mutate(date_clean = parse_date_time(date, orders = c('dmy','My','ymd')))
# intéressant. obligée de mettre M majuscule pour l'observation où il n'y a que le mois et l'année 

# enforce correct order ----

# it can happen that parse_date_time() does not detect that you prefer dmy over mdy (despite the correct order in the orders vector). in taht case, you have to brute-force your preference via the select_formats argument. this is a bit tricky and requires understanding how parse_date_time() works under the hood first.
# you see, parse_date_time() translates your orders like mdy into all kinds of versions of "month, day, year" using the notation %, eg %b-%d-Y. then, it tests these on your data. those formats that do not cuase an error on your dates are stored as the names of a vector called trained.
# finally, the default function that is passed to the select_formats argument assigns a score to each successful candidate and orders them accordingly. then, this order determines the preference that parse_date_time() chooses.
# we can take advantage of that by writing our own order function. in our case, we want all formats that start with the day (%d) to come first.
my_preference <- function(trained, drop = FALSE){
  # drop argument must be set but will not be used here
  preferenced_logical <- str_starts(names(trained), '%d')
  c(
    names(trained)[preferenced_logical],
    names(trained)[!preferenced_logical]
  )
}

# now, i can enforce my preference even if i don't sort the vector in the orders vector correctly
toy_data |> 
  mutate(parsed_wrong_order = parse_date_time(dates, orders = c('mdy', 'dmy')),
         parsed_order_enforced = parse_date_time(dates, orders = c('mdy', 'dmy'), select_formats = my_preference))
