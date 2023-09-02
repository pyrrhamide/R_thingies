
# https://alberts-newsletter.beehiiv.com/p/extract-time-signatures

# extracting days, months, etc. from a date!
# on connait ça hein
library(dplyr)
library(lubridate)

sp500_opens <- gt::sp500 |> select(date, open)
glimpse(sp500_opens)

# the tedious way ----
# = manually. on connait aussi...
sp500_opens |> 
  mutate(day = day(date), month = month(date), year = year(date))

# compute all time units all at once ----
# the timetk package is a neat package that is especially suited to work with time data. after all, it is designed to help you with time series analysis.
# among the useful functions from this package, you will find one function that takes care of all the previous function calls all at once. 
sp500_opens |> 
  timetk::tk_augment_timeseries_signature(date) |> 
  glimpse()
# ah ouais, c'est exhaustif...

# compute only desired time units ----
# based on the timetk function, here's a helper function that let's you actually select which time units you want

## function that extracts only desired time units
get_selected_time_signature <- function(data, date_var, time_units){
  data |> 
    timetk::tk_augment_timeseries_signature({{date_var}}) |> 
    select(
      all_of(
        c(colnames(data), time_units)
      )
    )
}

sp500_opens |> 
  get_selected_time_signature(
    date_var = date, 
    time_units = c('year', trimestre = 'quarter', 'month', 'day') 
    # top, on peut aussi renommer comme dans select() de base
  )

# what about non-tibble data? ----
# what if you want to use a vector as input and extract time units from this?
# there is another function tk_get_timeseries_signature() and it works just like you'd expect: you feed it with a vector of dates and it computes all the time units for these dates
timetk::tk_get_timeseries_signature(sp500_opens$date)
