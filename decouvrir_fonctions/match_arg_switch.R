
# Comprendre match.arg() et switch() à l'aide de ~*l'intelligence artificielle*~ 

# Avant de réécrire ce qu'elles m'ont expliqué, j'essaie de décrire avec mes mots :

## match.arg() => quand j'ai des choix précis à imposer dans une fonction, match.arg() se charge de vérifier & valider que l'input (une fois la fonction dans l'environnement) correspond à ce que je veux imposer. attention, c'est du partial matching, sensible à la casse, donc si une des options est "tomate", "toma" devrait passer et l'exécution de la fonction devrait se poursuivre.

## switch() => c'est chaud à expliquer. pour une entrée donnée, je lui fais correspondre UN ELEMENT précis, par exemple une chaîne de caractère, une fonction, un chiffre...le dernier élément est la "réponse" par défaut.

# ------- 1 : match.arg() ---------
# GPT 3.5 : RAS
sisi <- function(ouioui = c("bras", "main", "pied", "jambe")) {
  ouioui <- match.arg(ouioui)
  ouioui
}
sisi() # première option par défaut
sisi("pied")
sisi("j") # jambe -- correspo partielle 
sisi("t") # hop ! erreur 
sisi("MaIn") # erreur : la casse 

# GPT 4o : function used to validate an argument in custom function to ensure it matches of the allowed or expected values. if arg isn't explicitely specified, it also selects a default value from the provided options
# why use match.arg()
## 1. error checking: ensures only valid argument values are used
## 2. ease of use: supports partial matching, so users can type shorter inputs
## 3. default handling: automatically picks the first option as a default if no argument is provided
my_function <- function(type = c("tinky-winky", "dipsy", "lala", "po")){
  type <- match.arg(type)
  return(paste("You selected:", type))
}

my_function() # par défaut
my_function("dip") # partial match
my_function("po")
my_function("sandoval") # erreur 

# ------- 2 : switch() --------
# GPT 3.5 : if u have a limited set of options and want to perform different actions based on the value, u can consider using the switch() function 
# used for vectorized conditionnal branching. allows u to select one of several alternatives based on the value of a given expression 
louper_tournee_starac <- function(eleve){
  switch(
    eleve,
    "Maelys",
    "Paul",
    "Thomas",
    "Noah",
    "Emma",
    "Masséo",
    "Julie",
    "Au-delà, ils font la tournée !"
  )
}

louper_tournee_starac(4)
louper_tournee_starac("4") # ooooh "une EXPR numérique est requise dans 'switch' sans alternatives nommées" 
louper_tournee_starac(7) # :(
louper_tournee_starac(13) # pas dans le champ, donc pas de retour console (pas ce que GPT 3.5 disait...)

# GPT 4o : for cases where u want to map inputs to specific behaviours. control-flow function that allows u to execute different actions or return different values based on the value of an input. compact & efficient way to handle multiple conditions, similar to a "switch-case" statement in other programming languages.
# switch(EXPR, case1 = value1, case2 = value2, ...)
# if EXPR matches one of the case names, switch() returns or executes the corresponding value (valueX). if no match is found, NULL is returned by default, unless u specify a fallback case.
switch("apple",
       apple = "u selected a fruit",
       carrot = "u selected a veggie",
       potato = "u selected a root veggie") # même pas besoin de guillemets!!

switch(2,
       "eins",
       "zwei",
       "drei") # handling numeric input (indexing). EXPR as position index

switch("banana",
       apple = "u selected a fruit",
       carrot = "u selected a veggie",
       cli::cli_alert_danger("5 FRUITS ET LEGUMES PAR JOUR")) # fallback. default case by providing value without a name (usually at the end). in exemple is simple string, i added the cli() and it works too 

star_ac <- function(eleve){
  switch (eleve,
          ebony = "finale",
          marine = "finale",
          franck = "demi-finale",
          charles = "demi-finale",
          stop("et bah non, on prend que le top 4 ici")
  )
}

star_ac("ebony")
star_ac("charles")
star_ac("ulysse")

# useful when each option triggers a different behaviour, doesn't support partial matching
# can be a function!!! not just a string 

# concise alternative to if/else if/else chains for simple, discrete conditions
# limitations : no partial matching, no range checking (must specify each element individually), ambiguity w/ num & char inputs 

# nested if statement v switch()
# example scenario : meal selection
recommend_meal_if <- function(meal) {
  if (meal == "breakfast") {
    return("pancakes w/ syrup") 
  } else if (meal == "lunch") {
    return("grilled cheese sandwich")
  } else if (meal == "dinner") {
    return("spaghetti & meatballs")
  } else {
    return("invalid meal type. please choose breakie, lunch or dinner")
  }
}

recommend_meal_if("breakfast")
recommend_meal_if("dinner")
recommend_meal_if("gouter")

recommend_meal_switch <- function(meal) {
  switch(meal,
         breakfast = "pancakes w/ syrup",
         lunch = "grilled cheese sandwich",
         dinner = "spaghetti & meatballs",
         "invalid meal type. please choose breakie, lunch or dinner")
}

recommend_meal_switch("breakfast")
recommend_meal_switch("dinner")
recommend_meal_switch("gouter")
# hmmm et donc mélanger avec match.arg() permet de faire du partial matching aussi

recommend_meal_switch_match <- function(meal = c("breakfast", "lunch", "dinner")) {
  meal <- match.arg(meal)
  switch(meal,
         breakfast = "pancakes w/ syrup",
         lunch = "grilled cheese sandwich",
         dinner = "spaghetti & meatballs",
         "invalid meal type. please choose breakie, lunch or dinner")
}

recommend_meal_switch_match("break")
recommend_meal_switch_match("d")
recommend_meal_switch_match("gouter") # match.arg supplante le fallback 
