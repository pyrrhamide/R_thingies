### découvrir data.table ###

# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/

library(data.table)
library(tidyverse)

## importer table - petite, vitesse comparable à read_csv
mt <- fread("https://raw.githubusercontent.com/selva86/datasets/master/mtcars.csv")
head(mt)
class(mt)

## créer large table csv
set.seed(100)
m <- data.frame(matrix(runif(10e6), nrow=10e5))
write.csv(m, "m2.csv", row.names = F)

## time taken by base::read.csv to import
system.time({m_df <- read.csv('m2.csv')}) # 26.30s

## time taken by data.table::fread to import
system.time({m_dt <- fread('m2.csv')}) # ah ouais, vachement plus rapide (0.25s)

## pour convertir un df en dt
# data.table(df) ou as.data.table(df) -> creates a copy of df and converts to dt
# setDT(df) -> converts it to dt inplace (no need to assign to new object)

## reload data
data("mtcars")
head(mtcars)
mtcars$carname <- rownames(mtcars)

mtcars_dt <- as.data.table(mtcars)
# ou
mtcars_copy <- copy(mtcars)
class(mtcars_copy)
setDT(mtcars_copy)
class(mtcars_copy)

## filtering rows based on conditions
# base
mtcars[mtcars$cyl==6 & mtcars$gear==4,]
# dt
mtcars_dt[cyl==6 & gear==4,]

## selecting columns
mtcars[,1]
mtcars_dt[,1] # bah...ça fonctionne?
mtcars_dt[,1,with=F]
mtcars_dt[,mpg] # plus "propre"

## select multiple columns using a character vector
myvar <- "mpg"
mtcars_dt[,myvar]
mtcars_dt[,myvar,with = F]

columns <- c('mpg','cyl','disp')
mtcars_dt[,columns,with = F]

## select multiple columns DIRECTLY - .()
mtcars_dt[1:4,list(mpg,cyl,disp)]
mtcars_dt[1:4,.(mpg,cyl,disp)]

## drop columns - !
drop_cols <- c('mpg','cyl','disp')
mtcars_dt[1:4,!drop_cols,with = F]

## rename columns - setnames()
# changes the name without copying the data
setnames(mtcars_dt, "vs", "engine_type") # oldname newname
colnames(mtcars_dt)

(DT <- data.table(A=1:5))
DT[, X:=shift(A,1,type="lag")]
DT[, Y:=shift(A,1,type="lead")]
DT

## EXERCICE avec airquality dt
data("airquality")
head(airquality)
class(airquality)
  # peut sauter les trois lignes du dessus, mais pour être safe on fait tout bien
setDT(airquality)
airquality[!is.na(Ozone),.(Solar.R,Wind,Temp)]

## creating new column from existing columns - :=
# df syntax
mtcars_dt[1:5]
mtcars_dt$cyl_gear <- mtcars_dt$cyl + mtcars_dt$gear
# dt syntax
mtcars_dt[, cyl_gear2:=cyl+gear]
head(mtcars_dt)

mtcars_dt[, `:=`(cyl_gear3 = cyl*gear,
                 cyl_gear4 = cyl-gear)] # pour générer plusieurs variables

## creating new column using character vector
myvar <- c('var1')
mtcars_dt[, myvar:=1] # no bueno
# syntax 1
mtcars_dt[, c(myvar):=1]
# syntax 2
mtcars_dt[, (myvar):=2]

## deleting columns
mtcars_dt[, c("myvar", "var1") := NULL] # ah, obligatoire de mettre guillemets et d'écrire le c(), alors que pour sélectionner des colonnes on en a pas besoin

## EXERCICE
mtcars_dt[, mileage_type := ifelse(mpg>20,"high","low")]
head(mtcars_dt)

## grouping 
# mean mileage for each cylinder type
# base
aggregate(mpg ~ cyl, data=mtcars, mean)
# dplyr
mtcars |> group_by(cyl) |> summarise(mean(mpg))

# dt
mtcars_dt[,mean(mpg),cyl] # explicitement, "by = cyl"
# ou (pour donner un nom)
mtcars_dt[,.(mean_mileage=mean(mpg)),by=cyl]

# moyenne, par deux variables
mtcars_dt[,mean(mpg),.(cyl,gear)]

## special group by cases
# selecting the first/second/nth occuring value of mileage for each unique cyl
mtcars_dt[,mpg[1],cyl]
mtcars_dt[,mpg[2],cyl]

# last value
mtcars_dt[,mpg[length(mpg)],cyl]
mtcars_dt[,mpg[.N],cyl]

# .N = number of rows
mtcars_dt[,.N]
mtcars_dt[,.N,cyl]

# .I ?
mtcars_dt[, .I] # returns all the row numbers

# row numbers where cyl=6
mtcars_dt[cyl==6, .I] # internet dit non, car filtrage sur cyl=6 et donc row numbers ont changé 
mtcars_dt[, .I[cyl==6]]

## EXERCICE - nb of cars and mean mileage for gear type
names(mtcars_dt)
mtcars_dt[, .(.N, mean(mpg)), gear]
# ! soluce intéressante, par rapport à arrondi
mtcars_dt[, .(.N, mean(mpg) |> round(2)), gear]

## chaining: multiple dt operations one after the other, temp storage
dt1 <- mtcars_dt[, .(mean_mpg=mean(mpg),
                     mean_disp=mean(disp),
                     mean_wt=mean(wt),
                     mean_qsec=mean(qsec)), by=cyl]
dt1
output <- dt1[order(cyl), ]
output
# can be done by attaching another set of square brackets => "one step"
output <- mtcars_dt[, .(mean_mpg=mean(mpg),
                        mean_disp=mean(disp),
                        mean_wt=mean(wt),
                        mean_qsec=mean(qsec)), by=cyl][order(cyl),]
output

## writing functions in dt - .SD as first argument to lapply()
mtcars_dt[,.SD,cyl]
mtcars_dt[,lapply(.SD[,1:10, with=F],mean),cyl] # mean of vars 1 to 10, by cyl
# ou 
mtcars_dt[,lapply(.SD,mean),by=cyl,.SDcols = c("mpg","disp","hp","drat","wt","qsec")]

mtcars_dt[,lapply(.SD[,.(mpg,disp,wt,qsec)],mean),cyl][order(cyl)] # replicating "chaining" stuff

## keys: im guessing to join tables? à la "join x on ..."
setkey(mtcars_dt,carname) # by setting akey, dt gets sorted by that key
key(mtcars_dt) # to check key

dt1 <- mtcars_dt[,.(carname,mpg,cyl)] ; head(dt1)
dt2 <- mtcars_dt[1:10,.(carname,gear)] ; head(dt2)

dt1[dt2] # (inner) joins the tables on the key, carname

setkey(mtcars_dt,cyl,mpg) # for more than one key
head(mtcars_dt)

setkey(mtcars_dt,NULL) # to remove key(s)

(t1 <- mtcars_dt[,lapply(.SD[,.(mpg,disp,wt,qsec)],mean),cyl][order(cyl)])
# we can use "keyby" to order and set the "by" column in one go
(t2 <- mtcars_dt[,lapply(.SD[,.(mpg,disp,wt,qsec)],mean),keyby=cyl])
key(t2)

## join two or more dt
dt1 <- mtcars_dt[5:25,.(carname, mpg, cyl)] ; head(dt1)
dt2 <- mtcars_dt[1:10, .(carname, gear)] ; head(dt2)
dt3 <- mtcars_dt[2:12, .(carname, disp)] ; head(dt3)
