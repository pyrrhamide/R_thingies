# https://www.tidyverse.org/blog/2023/02/dplyr-1-1-0-vctrs/

# on découvre la fonction dplyr::consecutive_id()
library(dplyr)

transcript <- tribble(
  ~name, ~text,
  "Hadley", "I'll never learn Python.",
  "Davis", "But aren't you speaking at PyCon?",
  "Hadley", "So?",
  "Hadley", "That doesn't influence my decision.",
  "Hadley", "I'm not budging!",
  "Mara", "Typical, Hadley. Stubborn as always.",
  "Davis", "Fair enough!",
  "Davis", "Let's move on."
)

transcript

# we were working with this data and wanted a way to collapse each continuous thought down to one line. for example, rows 3-5 all contain a single idea from Hadley, so we'd like those to be collapsed into a single line. this isn't quite as straightforward as a simple group-by name and summarise():
transcript |> 
  summarise(text = stringr::str_flatten(text, collapse = " "), .by = name)

# this isn't quite right because it collapsed the first row alongside rows 3-5. we need a way to identify consecutive runs representing when a single person is speaking, which is exactly what consecutive_id() is for.
transcript |> 
  mutate(id = consecutive_id(name))

# consecutive_id() takes one or more columns and generate an integer vector that increments every time a value in one of those columns changes (bordel, ça veut dire que je peux pas faire un arrange() comme ça, sinon je perds l'ordre). this gives us something we can group on to correctly flatten our text.
transcript |> 
  mutate(id = consecutive_id(name)) |> 
  summarise(text = stringr::str_flatten(text, collapse = " "), .by = c(id, name)) 
# going by id alone is actually enough, but grouping by name is a convenient way to drag the name along into the summary table 
