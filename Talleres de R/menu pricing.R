###########################
###########################
#-Breve introduccion al R-#
###########################
###########################

#########################
#-Directorio de trabajo-#
#########################
getwd()
dir()
#setwd()

##############
#-Usar ayuda-#
##############

?sqrt
??normal

############
#-Paquetes-#
############

#Cargar paquete
library("ISLR")
install.packages("ISLR")

#Usar data set ubicado en un paquete
library("MASS")
Boston #Valor de las casas en los suburbios de Boston
head(Boston)

#############
#-Funciones-#
#############

#Utilizar funciones
sqrt(4) #Raiz cuadrada de 4
rnorm(100,0,1) #Simular 100 datos de una Normal(0,1)
names(Boston) #Nombres de las variables del data.frame llamado "Boston"

#Error en funciones
sqrt()
log(-2)
lm(medv~lstat)
rnorm(100,0,-3)

###################
#-Tipo de objetos-#
###################
x<-c(1,2,4,5,6,7)
x
class(x) #numeric

y=cbind(x,x)
y
class(y) #matrix

nombres=c("AA","BB","cc","de")
x=c(1,2,3,4)
g=data.frame(nombres,x)
g
class(g) #data.frame

###########################
###########################
#-Regresion lineal simple-#
###########################
###########################
?Boston
names(Boston)
head(Boston)
reg=lm(medv~lstat,data=Boston) #medv= "Mediana del valor de la casa" #lstat= "Porcentaje de hogares con nivel socioeconomico bajo"

reg
summary(reg)
class(reg)
str(reg) #Observamos la estructura del objeto reg

#Anova
anova(reg)

#Test lack of fit
table(Boston$lstat) #Hay repeticiones
install.packages("EnvStats")
library("EnvStats")
anovaPE(reg) #Nos estaria no rechazando el ajuste no lineal

#Nube de puntos y recta ajustada
plot (Boston$lstat,Boston$medv,main="Regresion lineal simple",xlab="% nivel socioeconomico bajo",ylab="Mediana del valor de las casas",pch=20)
abline(reg, col="red",lwd=2) #La relacion podria no ser lineal

########################
#-Regresion polinomica-#
########################
regpol=lm(medv~lstat+I(lstat^2),data=Boston)
summary(regpol)

plot (Boston$lstat,Boston$medv,main="Regresion polinomica",xlab="% nivel socioeconomico bajo",ylab="Mediana del valor de las casas",pch=20)
curve(regpol$coefficients[1]+regpol$coefficients[2]*x+regpol$coefficients[3]*x^2, col="red",lwd=2,add=T) #La relacion podria no ser lineal

##############################
##############################
#-Regresion lineal multiple.-#
##############################
##############################

#Datos provenientes de encuestas realizadas a consumidores de 168 restaurantes italianos en un New York. Los datos estan en la forma del punto de vista de los clientes
#Y = PRECIO
#X1 = PUNTAJE DE LA COMIDA
#X2 = PUNTAJE DE LA DECORACION
#X3 = PUNTAJE DEL SERVICIO
#X4 = VARIABLE INDICADORA 1 SI EL RESTAURANTE ESTA AL ESTE DE LA QUINTA AVENIDA

#Leemos los datos.

datos=read.csv("nyc.csv",header=T)
head(datos)
dim(datos)
str(datos)


#Sacamos la primer columna que cuenta los casos y la segunda que nos indica que restaurant es.

datos=datos[,c(-1,-2)]
head(datos)

#############################################################
##Parte a)## Desarrollar un modelo de regresión que predice directamente el precio de la cena (en dólares) usando un subconjunto o la totalidad de las 4 variables (x1,x2,x3,x4)
#############################################################

#Resolucion "directa"

reg1=lm(Price~Food+Decor+Service+East,data=datos)
reg1$coefficients
summary(reg1)

#Resolucion obteniendo las estimaciones de betas "manualmente" (X'X)^-1 *X'Y

X=as.matrix(cbind(1,datos[,c(2,3,4,5)])) #Nuestra matrix X tiene 1's en la primer columna
head(X)
Y=as.matrix(datos[,1])
betagorro=solve(t(X)%*%X)%*%t(X)%*%Y
betagorro

#############################################################
##Parte b)## Determinar cuáles de las variables (Food, Decor, Service, East) tiene un mayor efecto estimado sobre el precio? ¿Este efecto es también el más estadísticamente significativo?
#############################################################

#Calculemos el estadistico t para cada variable t= (betagorro - beta) / ErrorEstandar(Betagorro)

n=168 #Cantidad de obs
k=5 #Grados de no libertad (d+1)-> (4+1)
Ygorro=X%*%betagorro #Y=X*Beta
SCR= sum((Y - Ygorro)^2) #Σei^2
sigma=SCR/(n-k)
errorestandar=sqrt(sigma%*%diag(solve(t(X)%*%X))) # sigmagorro*diagonal de la matriz (X'X)^(-1)
t=betagorro/t(errorestandar)
pvalores=2*pt(-abs(t),n-k)

#O si no se puede ver en la salida de

summary(reg1)

##############################################################
##Parte c)## Si el objetivo es elegir la ubicación del restaurante donde el precio alcanzado se maximiza para la cena, ¿Donde debe estar el nuevo restaurante en el este o hacia el oeste de la Quinta Avenida?
##############################################################

summary(reg1) #Al este

##############################################################
##Parte d)## ¿Les parece posible lograr aumentar el precio estableciendo un nuevo servicio de alta calidad en Manhattan para los restaurantes italianos?
##############################################################

summary(reg1) #No, ya que no es significativa la variable "servicio" para la formacion del precio

##############################################################
##############################################################
#Quitando la variable servicio que no es significativa, nos queda un modelo de la forma

reg2=lm(Price~Food+Decor+East,data=datos)
summary(reg2)
