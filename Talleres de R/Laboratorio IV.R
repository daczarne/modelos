##############
#-Problema 1-#
##############
datos<-read.csv("problema1.csv",header=T)
datos$control<-as.factor(datos$control)
datos$levorphanol<-as.factor(datos$levorphanol)
datos$epinephrine<-as.factor(datos$epinephrine)

#Observamos la media de cada factor
tapply(datos$stress,list(datos$levorphanol,datos$epinephrine),mean,na.rm=TRUE)

#Observamos la cantidad de observaciones por factor
tapply(datos$stress,list(datos$levorphanol,datos$epinephrine),function(x) sum(!is.na(x)))

#Grafica para observar interaccion
install.packages("HH")
library("HH")
interaction2wt(stress ~ levorphanol+epinephrine, data=datos)

#Tabla ANOVA con aov
Anova1<-aov(formula = stress~levorphanol*epinephrine,data=datos)

#Tabla ANOVA con lm
mod<-lm(stress~levorphanol*epinephrine,data=datos)
anova(mod)

#Contraste de tuckey para comparar los niveles

Anova1<-aov(formula = stress~levorphanol+epinephrine,data=datos)
aa=glht(Anova1, linfct = mcp(levorphanol= "Tukey"))
summary(aa)
confint(aa)
plot(aa,main="levorphanol")

bb=glht(Anova1, linfct = mcp(especie= "Tukey"))
summary(bb)
confint(bb)
plot(bb,main="especie")



##############
#-Problema 3-#
##############

datos<-read.csv("problema3.csv",header=T)
head(datos)
str(datos)

#Observamos la media de cada factor
tapply(datos$min,list(datos$tipo.medico,datos$tipo.practica),mean,na.rm=TRUE)

#Observamos la cantidad de observaciones por factor
tapply(datos$min,list(datos$tipo.medico,datos$tipo.practica),function(x) sum(!is.na(x)))

#Grafica para observar interaccion
library("HH")
interaction2wt(min ~ tipo.medico+tipo.practica, data=datos)

#Tabla ANOVA con aov
Anova2<-aov(formula = min~tipo.medico*tipo.practica,data=datos)
Anova2

#Tabla ANOVA con lm
mod<-lm(min~tipo.medico*tipo.practica,data=datos)
anova(mod)

#Contraste de tuckey para comparar los niveles

Anova2<-aov(formula = min~tipo.medico+tipo.practica,data=datos)
aa=glht(Anova2, linfct = mcp(tipo.medico= "Tukey"))
summary(aa)
confint(aa)
plot(aa,main="tipo.medico")

bb=glht(Anova2, linfct = mcp(tipo.practica= "Tukey"))
summary(bb)
confint(bb)
plot(bb,main="tipo.practica")
