install.packages("MASS")
library(MASS)
set.seed(369)
n <- 10000
e<-rnorm(n,0,1)
b1 <- 1 
b2 <- -2
b0 <- 0


#####  SCENARIO 2  ######
C2<- matrix(c(1,0, 0, 1), nrow = 2, ncol = 2)
S2<- mvrnorm(n, mu = c(0, 0), Sigma=C2)
X<- S2[,1]
Y<- S2[,2]
reg <- b0 + b1*X + b2*Y + e
modS2<- lm(reg ~ X+Y)
summary(modS2)
### SIMULAZIONE DI MONTECARLO
nsim <- 1000
stima_S2_omesso <- numeric(nsim)
stima_S2_completo <- numeric(nsim)

for (i in 1:nsim) {
  set.seed(111 + i)
  S2 <- mvrnorm(n, mu = c(0, 0), Sigma = C2) 
  X <- S2[, 1]
  Y <- S2[, 2]
  e  <- rnorm(n, 0, 1)
  reg2 <- b0 + b1*X + b2*Y + e
  mod1 <- lm(reg2 ~ X)
  stima_S2_omesso[i] <- coef(mod1)["X"]
  mod2 <- lm(reg2 ~ X + Y)
  stima_S2_completo[i] <- coef(mod2)["X"]}

mean(stima_S2_omesso)
mean(stima_S2_completo)




#######   SCENARIO 1  ########
C1<- matrix(c(1,0.5, 0.5, 1), nrow = 2, ncol = 2)
S1<- mvrnorm(n, mu = c(0, 0), Sigma=C1)
X<- S1[,1]
Y<- S1[,2]
reg <- b0 + b1*X + b2*Y + e
modS1<- lm(reg ~ X+Y)
summary(modS1)
### SIMULAZIONE DI MONTECARLO
nsim <- 100000
stima_S1_omesso <- numeric(nsim)
stima_S1_completo <- numeric(nsim)

for (i in 1:nsim) {
  set.seed(111 + i)
  S1 <- mvrnorm(n, mu = c(0, 0), Sigma = C1) 
  X <- S1[, 1]
  Y <- S1[, 2]
  e  <- rnorm(n, 0, 1)
  reg1 <- b0 + b1*X + b2*Y + e
  mod1 <- lm(reg1 ~ X)
  stima_S1_omesso[i] <- coef(mod1)["X"]
  mod2 <- lm(reg1 ~ X + Y)
  stima_S1_completo[i] <- coef(mod2)["X"]}

mean(stima_S1_omesso)
mean(stima_S1_completo)
