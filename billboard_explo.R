library(tidyverse)
library(extrafont)
loadfonts(device = "win")

data("billboard")

billboard |> select(-c(starts_with("wk"))) |> summary()
billboard |> select(-c(starts_with("wk"))) |> map_dbl(n_distinct) #une chanson en double?

billboard |> 
  mutate(anmois=as.character(year(date.entered)*100+month(date.entered))) |> 
  count(anmois) |> 
  ggplot(aes(anmois,n)) + geom_col() +
  labs(x="Année mois",
       y="Nombre de chansons parues dans le classement",
       title="Chansons entrées dans classement Billboard 2000?") +
  theme(text = element_text(family = "Zilla Slab"))

## infos sur les chanteurs bb ----
t <- tibble(
  artist=c("Aaliyah","Jay-Z"),
  sex=c("F","M")
)

## Aaliyah ----
aaliyah <- billboard |> 
  filter(artist=="Aaliyah") |> 
  pivot_longer(wk1:wk76,names_to = "semaine",values_to = "rang") |> 
  mutate(sem=as.numeric(str_sub(semaine,3,length(semaine))),
         rang_rec=100-rang)

aaliyah

aaliyah |> map_dbl(n_distinct)

aaliyah |> filter(!is.na(rang)) |> 
  ggplot(aes(sem,rang_rec,group=track,colour=track)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Week",
    y = "Rank",
    title = "Billboard charts - Aaliyah",
    subtitle = "Year 2000",
    colour = "Track"
  )

## Jay-Z ----
jayz <- billboard |> 
  filter(artist=="Jay-Z") |> 
  pivot_longer(wk1:wk76,names_to = "semaine",values_to = "rang") |> 
  mutate(sem=as.numeric(str_sub(semaine,3,length(semaine))),
         rang_rec=100-rang) |> 
  left_join(t)

jayz

jayz |> map_dbl(n_distinct)

jayz |> filter(!is.na(rang)) |> 
  ggplot(aes(sem,rang_rec,group=track,colour=track)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Week",
    y = "Rank",
    title = "Billboard charts - Jay-Z",
    subtitle = "Year 2000",
    colour = "Track"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Karla"),
        legend.title = element_text(face = "bold"),
        legend.position = "top",
        plot.title = element_text(face = "bold"))


## le top 10 (3 finalement) artistes avec le plus de chansons? ----
top3 <- billboard |> count(artist) |> slice_max(order_by = n,n=3) |> select(artist)

t3 <- billboard |> inner_join(top3) |> 
  pivot_longer(starts_with("wk"),names_to = "semaine",values_to = "rang") |> 
  mutate(sem=as.numeric(str_sub(semaine,3,length(semaine))),
         rang_rec=100-rang)

t3 |> filter(!is.na(rang)) |> 
  ggplot(aes(sem,rang_rec,group=track,colour=artist)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Week",
    y = "Rank",
    title = "Billboard charts - top 3 artistes avec le plus de chansons classées",
    subtitle = "Year 2000",
    colour = "Artiste"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Karla"),
        legend.title = element_text(face = "bold"),
        legend.position = "top",
        plot.title = element_text(face = "bold"))
