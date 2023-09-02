# install.packages("yarrr")
library(yarrr)

?pirates
head(pirates)
class(pirates)
sapply(pirates, class)
sapply(pirates, n_distinct)

unique(pirates$favorite.pirate)
unique(pirates$fav.pixar)

aggregate(age ~ sex, pirates, mean)

par(mar=c(1,1,1,1)) # marges trop grandes otherwise

plot(pirates$height, pirates$weight)

plot(x = pirates$height,        # X coordinates
     y = pirates$weight,        # y-coordinates
     main = 'My first scatterplot of pirate data!',
     xlab = 'Height (in cm)',   # x-axis label
     ylab = 'Weight (in kg)',   # y-axis label
     pch = 16,                  # Filled circles
     col = gray(.0, .1))  
grid()

model <- lm(weight ~ height, pirates)
abline(model, col = 'blue')

pirateplot(age ~ sword.type, pirates, 
           pal = 'pony',
           main = "Pirate plot of ages by favourite sword")

# t-test : significant differences between ages of pirates who wear headband and other who don't?
t.test(age ~ headband, pirates, alternative = 'two.sided')

# correlation between height and weight?
cor.test(~ height + weight, pirates)
  # significant relationship between height and weight

# ANOVA: difference between number of tattoos and favourite sword?
tat.sword.lm <- lm(tattoos ~ sword.type, pirates)
summary(tat.sword.lm)
anova(tat.sword.lm)
  # significant: nb of tattoos are different based on favourite sword

# regression: age, weight, nb tattoos ~ nb treasure chests found?
tchest.model <- lm(tchests ~ age + weight + tattoos, pirates)
summary(tchest.model)
  # the older you are, the more treasure chests you find 

# Bayesian t-test comparing the age of pirates with or w/o headbands
BayesFactor::ttestBF(formula = age ~ headband, data = pirates)
  # Bayes factor of 0.12 = strong evidence for null hypothesis (that the mean age does not differ between pirates with or w/o headbands)


names(ToothGrowth) ; dim(ToothGrowth) ; str(ToothGrowth)
head(ToothGrowth[c("len", "supp")])

# subset! 
subset(ToothGrowth, subset = len < 20 & supp == "OJ" & dose >= 1)
subset(ToothGrowth, subset = len > 30 & supp == "VC", select=c(len, dose))
# = 
ToothGrowth[ToothGrowth$len < 20 & ToothGrowth$supp == "OJ" & ToothGrowth$dose>=1,]

# with!!
health <- data.frame("age" = c(32, 24, 43, 19, 43),
                     "height" = c(1.75, 1.65, 1.50, 1.92, 1.80),
                     "weight" = c(70, 65, 62, 79, 85))
health

# Calculate bmi
health$weight / (health$height ^ 2)
# = 
with(health, weight / (height ^ 2))

# Read a text file from the web
fromweb <- read.table(file = 'http://goo.gl/jTNf6P',
                      sep = '\t',
                      header = TRUE) #  ain't working, but I get the gist :)
