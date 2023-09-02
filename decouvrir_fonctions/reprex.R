# Créer des données reproductibles
runif(20)
runif(min=10, max=90, n=20)

rnorm(20)

rep(1:20,each=3)
rep(1:20,times=3)

sample(20)
sample(rep(1:20,times=3),10)

seq(1,20,by=2)

seq(0,10,by=.5)
(0:20)/2

sapply(3:9, seq)

formatC(c(1, 2, 56, 789), flag = "0", width = 4)
formatC(sample(45:999,20), flag="0", 5)

letters
LETTERS 

m <- matrix(1:10,nrow = 5,ncol = 2)
m
m[3,2]

n <- matrix(1:10,ncol = 5,byrow = TRUE)
n

o1 <- matrix(letters[1:15], nrow = 3, ncol = 5)
o1

rownames(o1) <- c("pierre","feuille","ciseaux")
colnames(o1) <- c("pouce", "index", "majeur", "annulaire", "auriculaire")

o1[,4]
o1[,"annulaire"]

data.frame(
  var1 = 1:10
  , var2 = letters[1:10]
  , var3 = rep(c(TRUE, FALSE), times = 5)
  , stringsAsFactors = FALSE
)
