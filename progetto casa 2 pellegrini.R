library(ggplot2)
library(glmnet)
library(corrplot)
library(RColorBrewer)

data(diamonds)

dati <- as.data.frame(ggplot2::diamonds)
names(dati) <- c("carati", "taglio", "colore", "purezza", "profondita_%", "tavola",
                 "prezzo", "lunghezza", "larghezza", "profondita")

# Aggiungiamo le variabili
set.seed(42)
dati$rumore <- round(rnorm(nrow(dati), mean = 1, sd = 4), 2)
dati$volume <- round(dati$lunghezza * dati$larghezza * dati$profondita, 2)

lista_continenti <- c("Africa", "Asia", "Sud_America", "Nord_America", "Europa", "Oceania")
set.seed(123)
dati$continente <- sample(lista_continenti, size = nrow(dati), replace = TRUE)
dati$continente <- as.factor(dati$continente)

set.seed(456)
dati$brillantezza <- as.numeric(dati$taglio) * 7 + rnorm(nrow(dati), mean = 10, sd = 5)
dati$brillantezza <- round(dati$brillantezza, 2)

set.seed(789)
dati$fluorescenza <- round(
  80 - (dati$prezzo / max(dati$prezzo) * 20) + rnorm(nrow(dati), mean = 0, sd = 7), 2
)
dati$fluorescenza <- pmax(0, pmin(100, dati$fluorescenza))

# Fattorizzo le variabili
dati$taglio <- as.factor(dati$taglio)
dati$colore <- as.factor(dati$colore)
dati$purezza <- as.factor(dati$purezza)
dati$continente <- as.factor(dati$continente)

# CORRELAZIONE E MULTICOLLINEARITA'
corr <- dati[, c("carati", "profondita_%", "tavola", "prezzo", "lunghezza", "larghezza", "profondita", "rumore", "volume", "brillantezza", "fluorescenza")]
matrice_cor <- round(cor(corr), 2)
print(matrice_cor)

palette_migliore <- colorRampPalette(c("#B2182B", "#D6604D", "#F7F7F7", "#4393C3", "#2166AC"))(200)
corrplot(matrice_cor, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", 
         col = palette_migliore)

# REGRESSIONE MULTIPLA CLASSICA (OLS)
reg1 <- lm(prezzo ~ ., data = dati)
summary(reg1)

# PREPARAZIONE MATRICI PER GLMNET
X <- model.matrix(prezzo ~ ., data = dati)[, -1]
Y <- dati$prezzo
mypal <- c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"), brewer.pal(12, "Paired"))

##### 1. REGRESSIONE RIDGE #####
fit.ridge <- glmnet(x = X, y = Y, family = "gaussian", alpha = 0)
cv.ridge <- cv.glmnet(x=X, y=Y, family="gaussian", alpha=0)

# Grafico Ridge
{plot(fit.ridge, xvar = "lambda", label = FALSE, main = "Ridge", col = mypal)
  abline(h = 0, col = "#FF9E77", lwd=2)
  abline(v = log(fit.ridge$lambda[c(50, 90)]), lty = "dashed", col = "grey60")
}

# Coefficienti Ridge (ERRORE BSIG CORRETTO QUI)
dim(fit.ridge$beta)
round(fit.ridge$lambda[c(1,50,100)], 5)
round(fit.ridge$beta[, c(1,50,100)], 5) 

# Confronto con OLS
coef(reg1)[c("lunghezza", "larghezza", "volume")]
fit.ridge$beta[c("lunghezza", "larghezza", "volume"), 50]

plot(cv.ridge)


##### 2. REGRESSIONE LASSO #####
fit.lasso <- glmnet(x = X, y = Y, family = "gaussian", alpha = 1)
cv.lasso <- cv.glmnet(x = X, y = Y, family = "gaussian", alpha = 1)

# Grafico Lasso (Stile Prof Viole Rosato)
{plot(fit.lasso, xvar = "lambda", label = FALSE, main = "Lasso", col = "#876197")
  abline(h = 0, col = "#FF9E77", lwd=2)
  abline(v = log(fit.lasso$lambda[c(50, 90)]), lty = "dashed", col = "grey60", lwd=1.5)
}

# Analisi Coefficienti Lasso
dim(fit.lasso$beta)
round(fit.lasso$lambda[c(1, 30, 60)], 5)
round(fit.lasso$beta[, c(1, 30, 60)], 5)

plot(cv.lasso)

# Lambda Ottimali Lasso
print(paste("Lasso Lambda Min:", cv.lasso$lambda.min))
print(paste("Lasso Lambda 1se:", cv.lasso$lambda.1se))


##### 3. ELASTIC NET E CONFRONTI #####
fit.elnet <- glmnet(x=X, y=Y, family="gaussian", alpha=0.5)
cv.elnet <- cv.glmnet(x=X, y=Y, family="gaussian", alpha=0.5)

# Plot Elastic Net (Stile Prof Viole Rosato)
{plot(fit.elnet, xvar="lambda", label=FALSE, main="Elastic Net", col="#876197")
  abline(h=0, col="#FF9E77", lwd=2)}
plot(cv.elnet)

# Confronto dei modelli (MSE minimo)
confronto_mse <- c(Ridge = min(cv.ridge$cvm), Lasso = min(cv.lasso$cvm), ElNet = min(cv.elnet$cvm))
print("Confronto Errore (MSE) tra i modelli:")
print(round(confronto_mse, 2))

# Estrazione coefficienti migliori
best_coeffs <- data.frame(
  Ridge = as.numeric(coef(cv.ridge, s="lambda.min")),
  Lasso = as.numeric(coef(cv.lasso, s="lambda.min")),
  ElNet = as.numeric(coef(cv.elnet, s="lambda.min")),
  row.names = rownames(coef(cv.ridge))
)
print("Coefficienti dei tre modelli a confronto:")
round(best_coeffs, 4)

##MULTI COLLINEARITA' DI TUTTI
# Calcoliamo prima i modelli e la relativa Cross Validation per tutti e tre
fit.ridge <- glmnet(x=X, y=Y, family="gaussian", alpha=0)
cv.ridge <- cv.glmnet(x=X, y=Y, family="gaussian", alpha=0)

fit.lasso <- glmnet(x=X, y=Y, family="gaussian", alpha=1)
cv.lasso <- cv.glmnet(x=X, y=Y, family="gaussian", alpha=1)

fit.EN <- glmnet(x=X, y=Y, family="gaussian", alpha=0.5)
cv.EN <- cv.glmnet(x=X, y=Y, family="gaussian", alpha=0.5)

# Prepariamo la palette usata nei suoi script
library(RColorBrewer)
mypal <- c(brewer.pal(12, "Set3"), brewer.pal(8, "Dark2"), brewer.pal(12, "Paired"))

# 1. Grafico RIDGE 
{plot(fit.ridge, xvar="lambda", label=FALSE, col=mypal, main="Ridge")
  abline(h=0, col="grey20")
  abline(v=log(cv.ridge$lambda.min), lty="dashed", col="#EA765B")
  abline(v=log(cv.ridge$lambda.1se), lty="dashed", col="#FC9F56")
}

# 2. Grafico LASSO
{plot(fit.lasso, xvar="lambda", label=FALSE, col=mypal, main="Lasso")
  abline(h=0, col="grey20")
  abline(v=log(cv.lasso$lambda.min), lty="dashed", col="#EA765B")
  abline(v=log(cv.lasso$lambda.1se), lty="dashed", col="#FC9F56")
}

# 3. Grafico ELASTIC NET
{plot(fit.EN, xvar="lambda", label=FALSE, col=mypal, main="Elastic Net")
  abline(h=0, col="grey20")
  abline(v=log(cv.EN$lambda.min), lty="dashed", col="#EA765B")
  abline(v=log(cv.EN$lambda.1se), lty="dashed", col="#FC9F56")
}

# 4. Confronto dei Test MSE dei tre modelli (usando il Lambda Minimo trovato con CV)
confronto_mse <- c(
  Ridge = min(cv.ridge$cvm), 
  Lasso = min(cv.lasso$cvm), 
  ElasticNet = min(cv.EN$cvm)
)

print("Confronto Errore (MSE): Quale modello predice meglio?")
print(round(confronto_mse, 2))

#VALORI LAMBDA SCELTI
# Estrazione dei valori di lambda ottimali (quelli che minimizzano l'MSE)
lambda_scelti <- c(
  Ridge = cv.ridge$lambda.min,
  Lasso = cv.lasso$lambda.min,
  ElasticNet = cv.elnet$lambda.min
)

print("Valori di Lambda ottimali scelti (lambda.min):")
print(round(lambda_scelti, 4))