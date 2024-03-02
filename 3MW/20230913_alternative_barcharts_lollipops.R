
### lollipops charts as alternatives to bar charts ###

# packages ----
library(tidyverse)

# bar chart first ----
mfr_counts <- gt::gtcars |> 
  count(mfr) |> 
  mutate(mfr = fct_reorder(mfr, n))

mfr_counts |> 
  ggplot() +
  geom_col(
    aes(x = n, y = mfr),
    fill = 'dodgerblue4'
  ) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Number of manufacturers in gt::gtcars"
  ) +
  theme_minimal(base_size = 16,
                base_family = "Nunito") +
  theme(panel.grid.minor = element_blank()) +
  # rapprocher les barres de l'axe des ordonnées
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))

# lollipop ----
# geom_col prints rectangles 
# must replace by lines with start (0) and end point (n)

mfr_counts |> 
  ggplot() +
  geom_segment(
    aes(x = 0, xend = n, y = mfr, yend = mfr),
    linewidth = 1,
    colour = 'dodgerblue4'
  ) +
  labs(
    x = element_blank(),
    y = element_blank(),
    title = "Number of manufacturers in gt::gtcars"
  ) +
  theme_minimal(base_size = 16,
                base_family = "Nunito") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()) +
  # rapprocher les barres de l'axe des ordonnées
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  # adding the points => lollipopping
  geom_point(aes(x = n, y = mfr), color = 'dodgerblue4', size = 3)

