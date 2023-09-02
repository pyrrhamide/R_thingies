# https://adv-r.hadley.nz/functionals.html

library(purrr)

# My first functional: map() ----
## Quickie ----
triple <- function(x) x * 3
map(1:5, triple)
# map refers to "an operation that associates each element of a given set with one or more elements of a second set"

## Producing atomic vectors ----
map_chr(mtcars, typeof)
map_lgl(mtcars, is.double)

n_unique <- function(x) length(unique(x))
map_int(mtcars, n_unique)

map_dbl(mtcars, mean)

# input vector must be same length as output vector, which implies that each call to .f must return a single value
pair <- function(x) c(x, x)
# map_dbl(1:2, pair)
# map_dbl(1:2, as.character)

# solution = go back to map(), to figure out what the problem is
map(1:2, pair) # ah oui, ça crée deux éléments pour un argument 
map(1:2, as.character)

## Anonymous functions and shortcuts ----
map_int(mtcars, \(x) length(unique(x))) 
  # ou map(., ~ length(unique(.x))) sauf que j'aime pas cette syntaxe
as_mapper(~ length(unique(.x)))

set.seed(1998)
x <- map(1:3, ~ runif(2))
str(x)

# purrr::pluck() powers extraction of elements from a vector, when using map functions
x <- list(
  list(-1, x = 1, y = c(2), z = "a"),
  list(-2, x = 4, y = c(5, 6), z = "b"),
  list(-3, x = 8, y = c(9, 10, 11))
)
x

# select by name 
map_dbl(x, 'x')
# or by position
map_dbl(x, 1)
# or by both 
map_dbl(x, list("y", 1))
# error if component doesn't exist
# map_chr(x, "z") # troisième liste n'a pas de z

## Passing arguments with ... ----
x <- list(1:5, c(1:10, NA))
map_dbl(x, \(x) mean(x, na.rm = TRUE))
# simpler (because map functions pass ... along). any arguments that come after f in the call to map() are inserted after the data in individual calls to f()
map_dbl(x, mean, na.rm = TRUE)

# ?
plus <- function(x, y) x + y
x <- rep(0, 5)
map_dbl(x, plus, runif(1))
map_dbl(x, ~ plus(.x, runif(1)))
# Hadley Wickham recommands writing the entire function rather than passing it along ..., as it makes it easier to read
# don't do
map(x, mean, 0.1)
# instead, do
map(x, \(x) mean(x, trim = 0.1)) # i really cannot with ~ i hate it

## Varying another argument ----
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

map_dbl(trims, ~ mean(x, trim = .x)) # le premier x est l'objet définit juste avant. le deuxième .x est l'entrée ! 
map_dbl(trims, \(trim) mean(x, trim = trim)) # plus explicite 

## Exercises ----
library(dplyr)
library(palmerpenguins)

# compute the sd of every column in a numeric data frame
map_dbl(mtcars, sd)
# compute the sd of every numeric column in a mixed data frame
penguins |> select(where(is.numeric)) |> map_dbl(sd, na.rm = TRUE)
# compute the number of levels for every factor in a data frame
penguins |> select(where(is.factor)) |> map(levels) # pas exactement ça
penguins |> select(where(is.factor)) |> map(n_distinct) # pas bon!!! sex

# the following code simulates the performance of a t-test for non-normal data. extract the p-value from each test, then visualise
trials <- map(1:100, ~ t.test(rpois(10, 10), rpois(7, 10)))

map(trials, names)
trials[[1]]$p.value
map_dbl(trials, `[[`, 3) # VICTOIRE!!! je vais pas visualise par contre, fuck it 

# use map() to fit linear models to the mtcars dataset using the formulas stored in this list:
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
map(formulas, \(formule) lm(formule, data = mtcars)) # omg

# fit the model mpg ~ disp to each of the bootstrap replicates of mtcars in the list below, then extract the R² of the model fit
bootstrap <- function(df) {
  df[sample(nrow(df), replace = TRUE), , drop = FALSE]
}

bootstraps <- map(1:10, ~ bootstrap(mtcars))
bootstraps

model <- map(bootstraps, \(rep) lm(mpg ~ disp, data = rep))
names(summary(model[[1]])) # on y va à taton parce que je me souviens plus de rien
summary(model[[1]])$r.squared

map_dbl(model, \(x) summary(x)$r.squared) # ...faut appliquer ça au travail maintenant 

# Purrr style ----
by_cyl <- split(mtcars, mtcars$cyl)
by_cyl

# we want to fit a linear model, then extract the second coefficient (i.e. the slope). the following code shows how you might to that with purrr
by_cyl |> 
  map(\(x) lm(mpg ~ wt, data = x)) |> 
  map(coef) |> 
  map_dbl(2)

# Map variants ----

# 23 primary variants of map(). Really 5 new ideas to get:
## - output same type as input with modify()
## - iterate over two inputs with map2()
## - iterate with an index using imap()
## - return nothing with walk()
## - iterate over any number of inputs with pmap()

## same type of output as input : modify() ----

# imagine you wanted to double every column in a data frame. you might first try using map() but map() always returns a list
df <- data.frame(
  x = 1:3,
  y = 6:4
)

map(df, ~ .x * 2)
# if you want to keep the output as a data frame, you can use modify(), which always returns the same type of output as the input
modify(df, \(x) x * 2) # returns a modified copy, does not modify the input!

## two inputs : map2() and friends ----

# map() is vectorised over a single argument, .x. this means it only varies .x when calling .f and all other arguments are passed along unchanged, thus making it poorly suited for some problems. for example, how would you find a weighted mean when you have a list of observations and a list of weights ? imagine we have the following data:
xs <- map(1:8, ~ runif(10))
xs[[1]][[1]] <- NA
ws <- map(1:8, ~ rpois(10, 5) + 1)

# you can use map_dbl() to compute the unweighted means
map_dbl(xs, mean)
# but passing ws as an additional argument doesn't work because arguments after .f are not transformed
map_dbl(xs, weighted.mean, w = ws) # erreur : 'x' et 'w' doivent avoir la même longueur

# we need a new tool: map2(), which is vectorised over two arguments. this means both .x and .y are varied in each call to .f:
map2_dbl(xs, ws, weighted.mean)
map2_dbl(xs, ws, weighted.mean, na.rm = TRUE)

# map2() recycles its inputs to make sure that they're the same lengths (LIES!!!!)
# in other words, map2(x, y, f) will automatically behave like map(x, f, y) when needed.

## no outputs : walk() and friends ----

# most functions are called for the value they return, so it makes sense to capture and store the value with a map function. but some functions are called primarily for their side-effects (e.g. cat(), write.csv(), ggsave(), addStyle(), tmtc...) and it doesn't make sense to capture their results.
welcome <- function(x){
  cat("Welcome ", x, "!\n", sep = "")
}
names <- c("Hadley", "Jenny")

map(names, welcome) # generates the welcomes, but also the return value of cat() (NULL)
walk(names, welcome)
# the outputs are ephemeral, and the input is returned invisibly

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

dir(temp)

## iterating over values and indices : imap() ----

# the imap() family allows you to iterate over the values and the indices of a vector in parallel
# imap() is like map2() in the sense that your .f gets called with two arguments, but here both are derived from the vector. imap(x, f) is equivalent to map2(x, names(x), f) is x has names, and map2(x, seq_along(x), f) if it does not.
# imap() is often useful for constructing labels:
imap_chr(iris, \(x, nom) paste0("the first value of ", nom, " is ", x[[1]]))

# if the vector is unnamed, the second argument will be the index
x <- map(1:6, ~ sample(1000, 10))
imap_chr(x, \(x, idx) paste0("the highest value of ", idx, " is ", max(x)))

# imap() is a useful helper if you want to work with the values in a vector along with their positions.

## any number of inputs : pmap() and friends ----

# instead of generalising map2() to an arbitrary number of arguments, purr takes a slightly different tack with pmap(): you supply it a single list, which contains any number of arguments. in most cases, that will be a list of equal-length vectors, ie something very similar to a data frame.
# there's a simple equivalence between map2() and pmap(): map2(x, y, f) is the same as pmap(list(x, y), f). the pmap() equivalent of map2_dbl(xs, ws, weighted.means) used above is:
pmap_dbl(list(xs, ws), weighted.mean, na.rm = TRUE)

# a big difference between pmap() and the other map functions is that pmap() gives you much finer control over argument matching because you can name the components of the list. returning to our example from section 9.2.5, where we wanted to vary the trim argument to x, we could instead use pmap():
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)

pmap_dbl(list(trim = trims), mean, x = x)

# it's often convenient to call pmap() with a data frame. a handy way to create that data frame is with tibble::tribble(), which allows you to describe a data frame row-by-row: thinking about the parameters to a function as a data frame is a very powerful pattern. the following example shows how you might draw random uniform numbers with varying parameters:
params <- tibble::tribble(
  ~ n, ~ min, ~ max,
  1L,     0,     1,
  2L,    10,   100,
  3L,   100,  1000
)

pmap(params, runif) # d'accoooord, en fait tu nommes les colonnes exactement comme les arguments de la fonction runif et donc ça fonctionne accordingly 

## exercises ----
modify(mtcars, 1) # remplace toutes les obs par la première ? intéressant...

# rewrite the following code to use iwalk() instead of walk2()
cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))

walk2(cyls, paths, write.csv)
iwalk(cyls, \())
