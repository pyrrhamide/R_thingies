
# packages 
library(dplyr)
library(ggplot2)
library(tidylog)

extrafont::loadfonts()

# clean data set 
penguins <- palmerpenguins::penguins |> filter(!is.na(sex))
glimpse(penguins)

# select colours 
colors <- thematic::okabe_ito()
# set names to species so that scale_fill_manual() will use these colours
names(colors) <- unique(penguins$species)

# create basic plot ----
basic_plot <- penguins |> 
  ggplot(aes(x = bill_length_mm, y = body_mass_g, fill = species)) +
  geom_point(shape = 21, size = 5, alpha = 0.85, color = "grey10") +
  theme_minimal(base_size = 16, base_family = "Zilla Slab")+
  scale_fill_manual(values = colors) +
  labs(
    x = "Bill length (in mm)",
    y = element_blank(),
    title = "Penguins from the Palmer Archipelago",
    subtitle = "Penguin weights (in g) for the species Adelie, Chinstrap and Gentoo",
    caption = "Data: {palmerpenguins} R package"
  ) +
  theme(
    plot.title.position = "plot", # ooooh! position extrême gauche
    text = element_text(color = "grey20"), # tout le texte en light grey
    axis.text = element_text(color = "grey30"),
    plot.title = element_text(
      family = "Zilla Slab SemiBold", size = 26, margin = margin(b = 7, unit = "mm")
    ),
    plot.subtitle = element_text(size = 18, lineheight = 1.3),
    panel.background = element_rect(color = "grey90"), # ew
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank() # pas mieux mais bon
  )

basic_plot

# instructions for colored words ----
# we've got the basic plot. time to add colour instructions into the title. and the way to do that is to use HTML span-tags that are wrapped aroung the targeted words, like so.
basic_plot +
  labs(
    subtitle = "Penguin weights (in g) for the species <span>Adelie</span>, <span>Chinstrap</span> and <span>Gentoo</span>"
  )

# these tags are HTML notation for using the wrapped part as an inline text. so in principle, adding them should change nothing.

# enable HTML notation ----
# the problem is that ggplot2 does not know that you want to use HTML notation. so you have to enable that. this is where element_markdown() from the ggtext() package comes into play.
# we can use element_markdown() in our theme() function and set plot.subtitle to element_markdown() as opposed to element_text() which we'd normally use.
basic_plot +
  labs(
    subtitle = "Penguin weights (in g) for the species <span>Adelie</span>, <span>Chinstrap</span> and <span>Gentoo</span>"
  ) +
  theme(plot.subtitle = ggtext::element_markdown(size = 18, lineheight = 1.3))
# this will render the span-tags instead of displaying them as text

# add color instructions ----
# now that we can use span-tags, we can change their style and set it to a specific color. the HTML code for this is very simple. it's style = "color: ...;". we will have to replace color with a hex code or known CSS color name (/!\ these are often not the same color name that R knows).
basic_plot +
  labs(
    subtitle = "Penguin weights (in g) for the species 
    <span style='color:#E69F00'>Adelie</span>, 
    <span style='color:#0072B2'>Chinstrap</span> and 
    <span style='color:#009E73'>Gentoo</span>"
  ) +
  theme(plot.subtitle = ggtext::element_markdown(size = 18, lineheight = 1.3),
        # remove legend as not needed anymore
        legend.position = "none")

# make the text bold (jure ya pas de cam chez moi ou dans ma tête, c'est fou) ----
# you might think that you can use all the fancy stuff from HTML in ggplot now. this would be cool, unfortunately ggtext does not implement all the styles from HTML.
# for example, you cannot use font-weight: bold (like in HTML) to make the words bold. this is not implemented. thankfully, for this specific use case, you can use the Markdown notation using **
# c'est dégueulasse wesh, je déteste mélanger du HTML, du Markdown...on dirait mon mémoire
basic_plot +
  labs(
    subtitle = "Penguin weights (in g) for the species 
    <span style='color:#E69F00'>**Adelie**</span>, 
    <span style='color:#0072B2'>**Chinstrap**</span> and 
    <span style='color:#009E73'>**Gentoo**</span>"
  ) +
  theme(plot.subtitle = ggtext::element_markdown(size = 18, lineheight = 1.3),
        # remove legend as not needed anymore
        legend.position = "none")

# clean up your code ----
# you may have noticed that it is very messy to throw all the span-tags into the subtitle argument of labs. also, manually setting the hex-codes in the span-tags is just an invitation to using the wrong color for one of the penguin species.
# so instead, i like to use the glue() function from the glue package to assemble the subtitle text with code and save it into a variable. then, one can use that variable in labs(). to me, this is a much safer approach.
title_text <- glue::glue(
  "Penguin weights (in g) for the Species",
  "<span style = 'color:{colors['Adelie']}'>**Adelie**</span>,",
  "<span style = 'color:{colors['Chinstrap']}'>**Chinstrap**</span> and",
  "<span style = 'color:{colors['Gentoo']}'>**Gentoo**</span>,",
  .sep = " "
)

title_text

basic_plot +
  labs(subtitle = title_text) +
  theme(plot.subtitle = ggtext::element_markdown(size = 18, lineheight = 1.3),
        # remove legend as not needed anymore
        legend.position = "none")
