

# packages 
library(tidyverse)
library(palmerpenguins)
library(gghighlight)

extrafont::loadfonts()

# setting up the plot ----
# histogram that shows distribution of penguin weights, per species
glimpse(penguins)

penguins_data <- penguins |> filter(!is.na(sex))

penguins_plot <- penguins_data |> 
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_histogram(position = 'identity', alpha = 0.7) +
  theme_minimal(base_size = 16) + # taille police 
  scale_fill_manual(values = thematic::okabe_ito(n = 3))

penguins_plot

# am quoting : in a histogram like this, it is a bit annoying that the bars overlap so much for different species. that makes it hard to concentrate on one species, and i even had to lower the transparency value alpha so that we can see anything at all. 
# one way to fix that is to give each species its own window.

penguins_plot + facet_wrap(vars(species))

# one crucial drawback : it's hard to compare the weights across the species because each window shows only one species.
# in order to give each species its own window and show the other data for context, we can use the {gghighlight} package. 

penguins_plot +
  facet_wrap(vars(species)) +
  gghighlight() # ohlala

# now we specify what part of the chart we want to highlight. the way to do that is to give conditions for the highlighted parts

penguins_plot +
  facet_wrap(vars(species)) +
  gghighlight(body_mass_g >= 3500)


# works with other types of charts ----

# Original data from https://ourworldindata.org/time-with-others-lifetime
import <- read_csv("3MW/données/time-spent-with-relationships-by-age-us.csv") |> 
  janitor::clean_names() |> 
  rename_with(\(x) str_remove(x, "time_spent_")) |> 
  rename_with(\(x) str_remove(x, "_by_age_of_respondent_united_states")) |> 
  rename_with(\(x) str_remove(x, "with_")) |> 
  rename(family = with_parents_siblings_and_other_family,
         age = year)

cleaned <- import |> 
  pivot_longer(alone:coworkers, names_to = "person", values_to = "minutes")

basic_line_chart <- cleaned |> 
  ggplot(aes(x = age, y = minutes, color = person)) +
  geom_line(linewidth = 1.5) +
  theme_minimal(
    base_size = 16,
    base_family = 'Nunito'
    ) +
  labs(
    x = 'Age',
    y = element_blank(),
    title = 'Around the age of 40, we spend less time with children\nand more time alone',
    subtitle = 'Daily time spent with others (in minutes)',
    caption = 'Data: Our World in Data'
  ) +
  scale_color_manual(values = thematic::okabe_ito(6))

basic_line_chart +
  gghighlight(person %in% c('alone', 'children')) # faut montrer ça à JS!!

basic_line_chart +
  gghighlight(person %in% c('alone', 'children'),
              age >= 38)

# finetuning the highlights ----

# error messages
# - weird grouping error message 
# - legend disappeared and small direct labels instead

basic_line_chart +
  gghighlight(person %in% c('alone', 'children'),
              age >= 38,
              use_direct_label = F,
              use_group_by = F)

# brings back the legend 

# setting the aesthetics of the unhighlighted parts via a list that you pass to unhighlighted_params

basic_line_chart +
  gghighlight(person %in% c('alone', 'children'),
              age >= 38,
              use_direct_label = F,
              use_group_by = F,
              unhighlighted_params = list(color = "grey70", linewidth = 1))

# j'essaie de répliquer le reste du mail à partir du site
# https://albert-rapp.de/posts/ggplot2-tips/07_four_ways_colors_more_efficiently/07_four_ways_colors_more_efficiently
# https://www.youtube.com/watch?v=nDa6aHxiSYQ

# sauvegarde de nos couleurs dans un vecteur
couleurs <- thematic::okabe_ito(2)

# titre
titre_texte <- glue::glue("Around the age of 40, we spend less time with <span style = 'color:{couleurs[2]}'>**children**</span><br> and more time <span style = 'color:{couleurs[1]}'>**alone**</span>")

basic_line_chart <- cleaned |> 
  ggplot(aes(x = age, y = minutes, color = person)) +
  geom_line(linewidth = 1.5) +
  theme_minimal(
    base_size = 16,
    base_family = 'Nunito'
  ) +
  labs(
    x = 'Age',
    y = element_blank(),
    title =  titre_texte,
    subtitle = 'Daily time spent with others (in minutes)',
    caption = 'Data: Our World in Data'
  ) +
  scale_color_manual(values = thematic::okabe_ito(6))

basic_line_chart +
  gghighlight(person %in% c('alone', 'children'),
              age >= 38,
              use_direct_label = F,
              use_group_by = F,
              unhighlighted_params = list(color = "grey70", linewidth = 1)) +
  # ligne pointillé
  geom_segment(x = 38, xend = 38, y = 0, yend = 275, linetype = 2, linewidth = 1, 
               colour = 'grey20')+
  annotate(
    'text',
    x = 80,
    y = c(425, 85),
    size = 6.5,
    label = c('alone', 'children'),
    family = 'Nunito',
    fontface = 'bold',
    hjust = 1,
    color = thematic::okabe_ito(2)
  ) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(), # enlève lignes verticales à mi-chemin
    panel.grid.major = element_line(linewidth = .5, linetype = 2), # pointillés
    plot.title.position = 'plot',
    text = element_text(color = 'grey20'),
    axis.text = element_text(color = 'grey40'),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = element_text(color = 'grey30', 
                                 size = rel(.8),
                                 margin = margin(b=8, unit='mm'))
    ) 
  # les mots dans le titre sont colorés grâce à couleurs, titre_texte et ggtext::element_markdown() !
  
# magnifico 
