#########################################
#-Laboratorio 3 Regresion Ridge y Lasso-#
#########################################

#Paquetes a utilizar

install.packages("ISLR")
install.packages("glmnet")
library(glmnet)
library(ISLR)

#¿Que hace la funcion model.matrix?

prueba=data.frame(x=c(1,0,0,1,0,0,0,1),y=c("a","b","c","d","a","b","c","d"),z=c(1.2,4.2,56,7,9,56,2.3,5))
head(prueba)
model.matrix(z~.,prueba) #Genera la matrix X con el vector de unos y las variables dummies

Hitters <- na.omit(Hitters) #Sacamos filas NA
head(Hitters)
x=model.matrix(Salary~.,data=Hitters)
head(x)
y= Hitters$Salary

###################
#-Regresion Ridge-#
###################

ridge=glmnet(x,y,alpha=0) #alpha=0 "ridge" alpha=1 "lasso"
#Por defecto toma distintos valores lambda automaticamente, podemos fijar el rango de los valores lambda, generalmente se utiliza:
plot(ridge)
secuencia=10^seq(10,-2,length=100)
ridge=glmnet(x,y,alpha=0,lambda = secuencia)
ridge
coefficients(ridge)#En filas tenemos los coeficientes y por columnas el valor de lambda

#Por defecto, estandariza las variables, podemos establecer que no lo haga
#ridge=glmnet(x,y,alpha=0,lambda=secuencia,standardize=F)

#Veremos que sucede a medida que disminuimos el valor de lambda

dim(coef(ridge))

ridge$lambda[50]
coef(ridge)[,50]
sqrt(sum(coef(ridge)[-1,50]^2))

ridge$lambda[60]
coef(ridge)[,60]
sqrt(sum(coef(ridge)[-1,60]^2))

#Observemos que pasa si lambda es 50

ridge50=glmnet(x,y,alpha=0,lambda = 50)
round(coef(ridge50),2)
sqrt(sum(coef(ridge50)^2))

#####
#-Grupo de validacion para la regresion Ridge
#####

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
train
test=(-train)
test

head(x[train,]) #Esta seria la muestra de entrenamiento
head(x[test,]) #Esta seria la muestra test

ridgetrain=glmnet(x[train,],y[train],alpha=0,lambda=secuencia)
ridgepred=predict(ridgetrain,newx=x[test,])

head(ridgepred) #Prediccion para la muestra test con el modelo conformado con la muestra de entrenamiento.
apply((ridgepred-y[test])^2,MARGIN = 2,FUN = mean)

apply((ridgepred-y[test])^2,MARGIN = 2,FUN = mean)[which.min(apply((ridgepred-y[test])^2,MARGIN = 2,FUN = mean))]

ridgetrain$lambda[64]

#Buscando el mejor lambda con validacion cruzada

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
mejorlambda=cv.out$lambda.min
mejorlambda
pred.conmejor.ridge=predict(ridge,s=mejorlambda,newx=x[test,])
mean((pred.conmejor.ridge-y[test])^2)

modelofinal=glmnet(x,y,alpha=0,lambda=mejorlambda)
coefficients(modelofinal)

#####ESTIMAR POR MINIMOS CUADRADOS
#####COMPARAR CON MEJOR RIDGE

###################
#-Regresion Lasso-#
###################

lasso=glmnet(x[train,],y[train],alpha=1,lambda=secuencia)
plot(lasso)

#Buscaremos el mejor lambda por validacion cruzada

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
mejorlambda=cv.out$lambda.min
pred.conmejor.lasso=predict(lasso,s=mejorlambda,newx=x[test,])
mean((pred.conmejor.lasso-y[test])^2)

lassofinal=glmnet(x,y,alpha=1,lambda=mejorlambda)
coefficients(lassofinal) #Vemos la ventaja de la regresion lasso donde elimina coeficientes.

#Compararemos el método de minimos cuadrados con el ridge y el lasso

mod.lm=lm(Salary~.,data=Hitters[train,])
pred.lm=predict.lm(mod.lm,Hitters[test,])
mean((pred.lm-y[test])^2)
mean((pred.conmejor.ridge-y[test])^2)
mean((pred.conmejor.lasso-y[test])^2)

plot(Hitters$Salary[test],pch=20)
points(pred.lm,col="red",pch=18)
points(pred.conmejor,col="green",pch=16)
legend(0,2500,c("y","ygorro MCO","ygorro Ridge"),col = c("black","red","green"),pch = c(20,18,16),cex=0.7)

