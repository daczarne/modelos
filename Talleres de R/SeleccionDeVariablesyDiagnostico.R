##############################
###-Seleccion de variables-###
##############################

install.packages("ISLR")
# Se carga la bilblioteca que tiene los datos a utilizar
library(ISLR)

# Datos de baseball, información:
?Hitters
fix(Hitters) # para ver los datos como en planilla
names(Hitters) # nombres de variables
dim(Hitters) # número de filas y columnas

# Eliminación de datos faltantes
summary(Hitters) # el resumen indica si hay NAs en cada variable
sum(is.na(Hitters$Salary)) # número de NAs en la variable "Salary"
Hitters <- na.omit(Hitters) # se reemplaza "Hitters" por la base completa
dim(Hitters)
sum(is.na(Hitters$Salary))

# Se carga la bilbioteca que tiene las funciones para
# hacer selección de variables/modelos.
library(leaps)

## 1. Selección del mejor subconjunto de cada tamaño ##
# (Best Subset Selection)

# Ajuste de todos los modelos (hasta 8 variables por defecto)
?regsubsets
regfit.full <- regsubsets(Salary ~ ., data=Hitters)
2^8 # número de modelos considerados
summary(regfit.full)
# con el argumento "nvmax" se puede ampliar la búsqueda
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
2^19 # número de modelos considerados como máximo
reg.summary <- summary(regfit.full)

# componentes del objeto que devuelve "regsubsets()"
names(reg.summary)
# R^2
reg.summary$rsq
reg.summary$bic

# Gráfico de las medidas de comparación entre los distintos modelos
# pdf('Figura1.pdf')
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Número de variables",
     ylab = "SCR", type = "o")
plot(reg.summary$adjr2, xlab = "Número de variables", ylab =
       "R^2 ajustado", type = "o")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
plot(reg.summary$cp, xlab = "Número de variables",
     ylab = "Cp", type = 'o')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10],col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Número de variables",
     ylab = "BIC", type = 'o')
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)
# dev.off()

# Grafica una tabla de modelos mostrando
# cuales variables están en cada modelo.
# Los modelos están ordenados por el
# estadístico especificado.
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# Coeficientes estimados para el mejor
# modelo según BIC:
coef(regfit.full, 6)

## 2. Métodos secuenciales hacia adelante y atrás ##
# (Forward and Backward Stepwise Selection)

# 2.1 "Forward Selection" #
regfit.fwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19,
                         method = "forward")
summary(regfit.fwd)

# 2.2 "Backward Elimination" #
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19,
                         method = "backward")
summary(regfit.bwd)

# Comparación de los coeficientes estimados de los tres mejores
# modelos con 7 variables según cada uno de los tres métodos:
cbind(SubSel = sort(names(coef(regfit.full, 7))),
      Forward = sort(names(coef(regfit.fwd, 7))),
      Backward = sort(names(coef(regfit.bwd, 7))))

##Algoritmo alternativo de los metodos forward y backward

#Backward
step(lm(Salary~.,data=Hitters),direction = "backward")
#Forward
step(lm(Salary~+1,data=Hitters),scope=~AtBat + Hits + HmRun + Runs + RBI + Walks + Years + CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + League + Division + PutOuts + Assists + Errors + NewLeague,direction = "forward")
#Both
step(lm(Salary~+1,data=Hitters),scope=~AtBat + Hits + HmRun + Runs + RBI + Walks + Years + CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + League + Division + PutOuts + Assists + Errors + NewLeague,direction = "both")
step(lm(Salary~.,data=Hitters),scope=~AtBat + Hits + HmRun + Runs + RBI + Walks + Years + CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + League + Division + PutOuts + Assists + Errors + NewLeague,direction = "both")







######################################################################################
#-Eligiendo entre modelos a través de un conjunto de validación y validación cruzada-#
######################################################################################

###########################################################################
#-Conjunto de validacion muestra de entrenamiento - muestra de validación-#
###########################################################################

set.seed(1) # se fija la semilla para que los resultados sean reproducibles
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ],
                          nvmax = 19)
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
val.errors<- rep(NA, 19)

for(i in 1:19){
  coefi <- coef(regfit.best, id = i) #iterativamente probamos el error fuera de la muestra con cada "mejor modelo", (k=1,k=2,...)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)
plot(val.errors, type = 'o')
points(which.min(val.errors), val.errors[which.min(val.errors)],
       col = 'red', pch = 19)

# coeficientes estimados para el modelo con error más bajo
coef(regfit.best, 10)

# luego se busca el mejor modelo con 10 variables sobre el
# conjunto de datos completo (no tiene por qué dar igual)
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

######################
#-validación cruzada-#
######################

# se define una función para hacer la predicción
# (porque la función "regsubsets()" no tiene método asociado)
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# validación cruzada "10-fold"
k <- 10
set.seed(1) # se fija la semilla para que los resultados sean reproducibles
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# implementación de validación cruzada
for(j in 1:k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds !=j, ],
                         nvmax = 19)
  for(i in 1:19){
    pred <- predict.regsubsets(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <- mean( (Hitters$Salary[folds == j] - pred)^2)
  }
}

# errores obtenidos en cada iteración
cv.errors
# promedio de errores por mejor modelo de cierta cantidad de variables
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# gráfico de promedios
plot(mean.cv.errors, type = 'o')
# modelo que tiene menor error promedio
which.min(mean.cv.errors)
# distribución de los errores
boxplot(cv.errors)
# parecen haber modelos con desempeños "similares"

# nuevamente se ajusta el modelo sobre el conjunto de datos
# completo y se observa el del tamaño adecuado según VC
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)







##############################
###-Diagnostico del modelo-###
##############################

library(MASS)

head(Boston)

par(mfrow=c(1,1))

plot (Boston$lstat,Boston$medv,main="Regresion lineal simple",xlab="% nivel socioeconomico bajo",ylab="Mediana del valor de las casas",pch=20)
reg=lm(medv~lstat,data=Boston)
summary(reg)
abline(reg, col="red",lwd=2) #La relacion podria no ser lineal

##Residuos##

residuos=reg$residuals
plot(Boston$lstat,residuos,main="Residuos vs Valor de la variable regresora",xlab="lstat",ylab="Residuos",pch=20)

##Residuos studentizados##

ri = rstandard(reg) ## Residuos studentizados
rii = residuals(reg)/(summary(reg)$sig*sqrt(1-hatvalues(reg))) #calculo manual
head(cbind(ri,rii))

plot(Boston$lstat,ri,,main="Residuos studentizados vs Valor de la variable regresora",xlab="lstat",ylab="Residuos",pch=20)
abline(h=0,col="red",lwd=2)

plot(reg)

## Normalidad de los residuos
par(mfrow=c(1,3))
hist(residuals(reg), probability=T)
lines(density(residuals(reg)))
boxplot(residuals(reg))
qqnorm(residuals(reg)) ; qqline(residuals(reg), col = 2)

## Tests
# Shapiro WIlk
shapiro.test(residuals(reg))

#Jarque Bera
install.packages("tseries")
library(tseries)
jarque.bera.test(residuals(reg))

# Anderson Darling
install.packages("nortest")
library(nortest)
ad.test(residuals(reg))

##########################################
#-Ajustamos con una regresion polinomica-#
##########################################

regpol<-lm(medv~lstat+I(lstat^2),data=Boston)
summary(regpol)

plot (Boston$lstat,Boston$medv,main="Regresion polinomica",xlab="% nivel socioeconomico bajo",ylab="Mediana del valor de las casas",pch=20)
curve(regpol$coefficients[1]+regpol$coefficients[2]*x+regpol$coefficients[3]*x^2, col="red",lwd=2,add=T)

##Residuos##

residuospol=regpol$residuals
plot(Boston$lstat,residuospol,main="Residuos vs Valor de la variable regresora",xlab="lstat",ylab="Residuos",pch=20)

##Residuos studentizados##

ri = rstandard(regpol) ## Residuos studentizados
plot(Boston$lstat,ri,,main="Residuos studentizados vs Valor de la variable regresora",xlab="lstat",ylab="Residuos",pch=20)
abline(h=0,col="red",lwd=2)

plot(regpol)

## Normalidad de los residuos
par(mfrow=c(1,3))
hist(residuals(regpol), probability=T)
lines(density(residuals(regpol)))
boxplot(residuals(regpol))
qqnorm(residuals(regpol)) ; qqline(residuals(regpol), col = 2)

## Tests
# Shapiro WIlk
shapiro.test(residuals(regpol))

#Jarque Bera
install.packages("tseries")
library(tseries)
jarque.bera.test(residuals(regpol))

# Anderson Darling
install.packages("nortest")
library(nortest)
ad.test(residuals(regpol))










##############################
#-Outliers y Obs.Influyentes-#
##############################

## (d) h_ii
## (e) R-student(ti), rstudentizados(ri)
dim(datos)
p = 3
n = 14

### OUTLIERS

## Prueba outlier
# Estadístico de prueba (residuo R-Student - ti)
# tcritico = qt(0.975,n-p-1)
ti2 = rstudent(m2)
ti2
abs(ti2) > qt(0.975, n - p - 1) ## Obs 13 outlier en media
ti2 > qt(0.95, n - p - 1)       ## Obs 14 outlier en varianza


### INFLUYENTES
ri2 = rstandard(m2) ## Residuos studentizados
cbind(y = datos$y, yhat = m2$fitted, resid = m2$resid,ri = ri2, ti = rstudent(m2), hii = hatvalues(m2))


### Veo si la obs 13 es influyente sobre la capacidad predictiva (DFFIT)

2/(sqrt(p/n)) # (4.320494)
abs(dffits(m2)) > 4.320494
## Ninguna observación onfñuyente sobre capacidad predictiva.

### Veo si la obs 13 es influyente sobre la estimacion del modelo(DFBETA)

2/sqrt(n) # (0.5345225)
dfbetas(m2) ## abs(dfbetas(m2)) > 0.5345225...
abs(dfbetas(m2)) > 0.5345225
## Obs13 y Obs14 influyente sobre beta1 y beta2


## Distancia de Cook
F_med = qf(0.5,n,n-p)
cooks.distance(m2) > F_med
## Ninguna influyente.

## covratio
1+(3*p)/n
covratio(m2) > 1+(3*p)/n | covratio(m2) < 1-(3*p)/n

influence(m2)
influence.measures(m2)






read.csv("nyc.csv")















# Eligiendo entre modelos a través de un
# conjunto de validación y validación cruzada

# muestra de entrenamiento - muestra de validación

set.seed(1) # se fija la semilla para que los resultados sean reproducibles
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary ~ ., data = Hitters[train, ],
                          nvmax = 19)
test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])
val.errors<- rep(NA, 19)

for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)
plot(val.errors, type = 'o')
points(which.min(val.errors), val.errors[which.min(val.errors)],
       col = 'red', pch = 19)

# coeficientes estimados para el modelo con error más bajo
coef(regfit.best, 10)

# luego se busca el mejor modelo con 10 variables sobre el
# conjunto de datos completo (no tiene por qué dar igual)
regfit.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

# validación cruzada

# se define una función para hacer la predicción
# (porque la función "regsubsets()" no tiene método asociado)
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# validación cruzada "10-fold"
k <- 10
set.seed(1) # se fija la semilla para que los resultados sean reproducibles
folds <- sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# implementación de validación cruzada
for(j in 1:k){
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds !=j, ],
                         nvmax = 19)
  for(i in 1:19){
    pred <- predict(best.fit, Hitters[folds == j, ], id = i)
    cv.errors[j, i] <- mean( (Hitters$Salary[folds == j] - pred)^2)
  }
}

# errores obtenidos en cada iteración
cv.errors
# promedio de errores por mejor modelo de cierta cantidad de variables
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
# gráfico de promedios
plot(mean.cv.errors, type = 'o')
# modelo que tiene menor error promedio
which.min(mean.cv.errors)
# distribución de los errores
boxplot(cv.errors)
# parecen haber modelos con desempeños "similares"

# nuevamente se ajusta el modelo sobre el conjunto de datos
# completo y se observa el del tamaño adecuado según VC
reg.best <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
coef(reg.best, 11)
