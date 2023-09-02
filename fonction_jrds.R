library(magrittr)
library(questionr)

d <- data.frame(
  id = 1:20,
  x = rep(1:5),
  y = rep(1:4),
  z = runif(20, min = 0, max = 100)
)

brut <- function(data,x,y) {
  tab <- data %$%
    table({{x}},{{y}}) %>% 
    lprop(n = TRUE)
  
  lignes <- nrow(tab)
  colonnes <- ncol(tab)
  colnames(tab)[colonnes -1] <- "% tot."
  colnames(tab)[colonnes] <- "Effectifs"
  
  return(tab)
}

brutbrut <- function(data,x,y) {
  tab <- data %$%
    wtd.table(x = {{x}},y = {{y}}) %>%
    lprop(n = TRUE)
  
  lignes <- nrow(tab)
  colonnes <- ncol(tab)
  colnames(tab)[colonnes-1] <- "% tot."
  colnames(tab)[colonnes] <- "Effectifs"
  
  return(tab)
}

lprop_pctot <- function(data, x, y, pond = NULL, tot_pond = FALSE, 
                        num = TRUE, ch = 1, nr = c("no","ifany","always")) {
  if (is.null({{pond}})) {
    tab <- data %$%
      table({{x}},{{y}},useNA = nr) %>%
      lprop(n = TRUE, digits = ch)
    
    lignes <- nrow(tab)
    colonnes <- ncol(tab)
    colnames(tab)[colonnes - 1] <- "% tot."
    colnames(tab)[colonnes] <- "Effectifs"
  }
  else {
    weights <- data$pond
    
    tab <- data %$%
      wtd.table(x = {{x}},y = {{y}}, weights = weights, useNA = nr) %>%
      lprop(n = TRUE, digits = ch)
    lignes <- nrow(tab)
    colonnes <- ncol(tab)
    colnames(tab)[colonnes-1] <- "% tot."
    colnames(tab)[colonnes] <- "Effectifs"
    
    if (tot_pond == FALSE) {
      tab[,colonnes] <- data %$%
        table({{x}},{{y}}, useNA = nr) %>%
        lprop(n = TRUE, digits = ch) %>%
        as.data.frame.array() %>%
        select(n)
    }
    else tab[,colonnes] <- round(tab[,colonnes])
  }
  for (i in 1:lignes) {
    tab[i,colonnes-1] <- round(100*tab[i,colonnes]/tab[lignes,colonnes],ch)
  }
  # if (num == TRUE) {
  #   tab <- tab[,colonnes(1:(colonnes-2),colonnes,colonnes-1)]
  #   return(tab)
  # }
  # if (num == FALSE) {
  #   tab <- tab[,-colonnes]
  #   return(tab)
  # }
  return(tab)
}
