############################
######-LABORATORIO VI-######
############################


##################################
##-Ejemplo 1 Regresion Binomial-##
##################################

#Leemos los datos

invest<-read.table("invest.txt", header=T)
summary(invest) #resumen de variables
attach(invest) #para llamar directamente a las variables
Pais #ejemplo de como llamar a las variables
invest$InvGrade<-as.factor(invest$InvGrade) #pasamos a factor la variable de respuesta
plot(invest) #ploteo de todas las variables 2 a 2

#Ajustamos el modelo

salida1<-  glm(InvGrade ~  IndicePais + DPBI+ PBIpercapita+ Inflacion + CrecimPBI, family=binomial, data=invest)
summary(salida1)

#Calculomos las predicciones

scores1<-predict.glm(salida1,type='response') #nos da el calculo del log-odd
scores1
SCOR1<-vector('character',length=length(InvGrade)) #generamos un vector vacio

for (i in 1:nrow(invest))
	if (scores1[i]>0.5) SCOR1[i]<-'1' else SCOR1[i]<-'0' #adjudicamos 1 si es mayor que 0.5

#Creamos tabla con errores de clasificacion

table( SCOR1, InvGrade)

#Ajustamos nuevo modelo

salida2<-  glm(InvGrade ~ PBIpercapita+ Inflacion, family=binomial, data=invest)
summary(salida2)

#Calculamos las predicciones

scores2<-predict(salida2,type='response')
SCOR2<-vector('character',length=length(InvGrade))

for (i in 1:nrow(invest))
	if (scores2[i]>0.5) SCOR2[i]<-'0' else SCOR2[i]<-'1'

#Creamos tabla con errores de clasificacion

table( SCOR2, InvGrade)

###################################
##-Ejemplo 2 Regresion Binomial-###
###################################
rm(list=ls())
#Leemos los datos
datos <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

#admit="se admite al alumno"
#gre= "historial de resultados de examenes" 
#gpa= "promedio de calificaciones"
#rank= "prestigio de la institucion donde se graduo"


#Resumen de los datos
summary(datos)
plot(datos)

#Tabla cruzada
po=xtabs(~admit + rank, data = datos)
po
po[2,]/(po[1,]+po[2,])*100 #Nivel de admision por ranking

#Transformamos a factor la variable de respuesta y el ranking
datos$rank <- factor(datos$rank)
datos$admit<- factor(datos$admit)

# Partimos la muestra en entrenamiento (70%) y validacion (30%)
dt = sort(sample(nrow(datos), nrow(datos)*.7))
train<-datos[dt,]
val<-datos[-dt,]

#Chequeamos el numero de filas de entrenamiento y validacion
nrow(train)
nrow(val)

#Regresion logistica sobre la muestra de entrenamiento
logistic <- glm(admit ~ ., data = train, family = "binomial")
summary(logistic)

#Stepwise sobre el modelo
logit = step(logistic)
summary(logit)

#Guardamos los coeficientes y calculamos intervalos de confianza
summary.coeff0 = summary(logit)$coefficient
round(confint(logit),2)

#Calcularemos los coeficientes estimados para el Odd Ratio
OddRatio = exp(coef(logit))
(summary.coeff = cbind(Variable = row.names(summary.coeff0), OddRatio, summary.coeff0))
row.names(summary.coeff) = NULL

#Estandarizacion de coeficientes
estandarizar <- function (regmodel)
{ b <- summary(regmodel)$coef[-1,1]
sx <- sapply(regmodel$model[-1], sd)
beta <-(3^(1/2))/pi * sx * b
return(beta)
}

std.Coeff = data.frame(Standardized.Coeff = estandarizar(logit))
std.Coeff = cbind(Variable = row.names(std.Coeff), std.Coeff)
row.names(std.Coeff) = NULL

#Resumen final
final = merge(summary.coeff, std.Coeff, by = "Variable", all.x = TRUE)
final

#Prediccion
pred = predict(logit, val, type = "response") #Prediccion utilizando la muestra de validacion
finaldata = cbind(val, pred)
finaldata

#Guardamos las predicciones en formato prediction
library(ROCR)
pred_val <-prediction(pred ,finaldata$admit)

# Plot the ROC curve
perf_val2 <- performance(pred_val, "tpr", "fpr") #FP vs TP segun probabilidad de positivo
plot(perf_val2, col = "green", lwd = 1.5)

#######################
#-Ejemplo multinomial-#
#######################
rm(list=ls())
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


#Leemos los datos
ml <- read.dta("http://www.ats.ucla.edu/stat/data/hsbdemo.dta")
summary(ml)


#200 estudiantes, se modeliza el tipo de programa que asiste el 
   #estudiante segun su situacion socioeconomica y su nivel de escritura

#female = 1 si es mujer
#ses = Estado socioeconomico
#schtype = tipo de escuela
#prog = Tipo de programa (variable a explicar)
#read = puntaje en lectura
#write = puntaje en escritura
#math = puntaje en matematica
#science = puntaje en ciencias

#Describimos por arriba a los datos
head(ml)
with(ml, table(ses, prog))
with(ml, do.call(rbind, tapply(write, prog,
    function(x) c(Media = mean(x), SD = sd(x))))) #la media de la escritura segun tipo de programa

#Ajustamos el modelo multinomial
ml$prog2 <- relevel(ml$prog, ref = "academic") #ponemos al nivel "academic" como referencia
test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)

#Test de wald
#estadistico z
z <- summary(test)$coefficients/summary(test)$standard.errors
z

#pvalor
p <- (1 - pnorm(abs(z), 0, 1))*2
p

#Vemos los coeficientes para el odd ratio
exp(coef(test))

head(pp <- fitted(test)) #Observamos las probabilidades ajustadas para nuestras obs


#Veremos las probabilidades ajustadas fijando cada variable
#Dejando fija la variable write en su media
dses <- data.frame(ses = c("low", "middle", "high"),
                   write = mean(ml$write))   
dses
ppses<- cbind(dses,predict(test, newdata = dses, "probs"))   
ppses

#Dejando fija la variable ses y tomando el recorrido de write
dwrite <- data.frame(ses = rep(c("low", "middle", "high"), each = 41), 
                     write = rep(c(30:70), 3))   #dejamos fija la variable ses, cambiando write
pp.write <- cbind(dwrite, predict(test, newdata = dwrite, type = "probs", se = TRUE))
pp.write

#Calculamos la media de las probabilidades para cada nivel ses
by(pp.write[, 3:5], pp.write$ses, colMeans) 

#Agregamos en un mismo data.frame los distintos niveles de ses, de write y las probabilidades
lpp <- melt(pp.write, id.vars = c("ses", "write"), value.name = "probability")
lpp

#Realizamos un ggplot para visualizar el anterior data.frame
ggplot(lpp, aes(x = write, y = probability, colour = ses)) +
  geom_line() +
  facet_grid(variable ~ ., scales="free")  

##Vemos como a mayor nivel de escritura y mayor
#nivel socioeconomico la probabilidad de tener un programa academico aumenta, 
#lo contrario a un programa vocacional

#####################
##-Ejemplo Poisson-##
#####################

require(ggplot2)
require(sandwich)
install.packages("msm")
require(msm)

#Leemos los datos
p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")

#Describimos los datos
head(p)
summary(p)

#Adjudicamos iterativamente los niveles del factor "prog"
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

#Calculamos la media y desvio del numero de premios segun programa
with(p, tapply(num_awards, prog, function(x) {
  sprintf("Media (SD) = %1.2f (%1.2f)", mean(x), sd(x))  #Media de premios por tipo de programa
}))

#Graficamos el numero de premios segun programa
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

##Ajuste del modelo
summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

##--Para estimar el desvio de forma robusta

#cov.m1 <- vcovHC(m1, type="HC0")
#std.err <- sqrt(diag(cov.m1))
#r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
#               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
#               LL = coef(m1) - 1.96 * std.err,
#               UL = coef(m1) + 1.96 * std.err)
#
#r.est
#
#with(m1, cbind(res.deviance = deviance, df = df.residual,
#               p = pchisq(deviance, df.residual, lower.tail=FALSE)))


## ajustamos el modelo quitando la variable prog
summary(m2 <- glm(num_awards ~ math, family="poisson", data=p))

## vemos si es significativo el modelo con la variable prog con chi cuadrado
anova(m2, m1, test="Chisq")

#Dejamos fija la variable "math" en su media y variamos prog, predecimos.
(s1 <- data.frame(math = mean(p$math),
                  prog = factor(1:3, levels = 1:3, labels = levels(p$prog))))

predict(m1, s1, type="response", se.fit=TRUE)


## calculamos y guardamos las predicciones
p$phat <- predict(m1, type="response")

## ordenamos la matriz segun programa y puntaje en matematicas
p <- p[with(p, order(prog, math)), ]
p

## Creamos el grafico con los valores predichos para las observaciones
ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
    geom_line(size = 1) +
  labs(x = "Math Score", y = "Expected number of awards")
