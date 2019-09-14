# library(grid)
# library(multcompView)
# library(mvtnorm)
# library(splines)
# library(survival)
# library(lattice)
library(multcomp)
# library(leaps)
library(HH)


rend=read.csv("trees.csv", header=TRUE)
dim(rend)
names(rend)
head(rend)

#An?lisis descriptivo
summary(rend)

#Transformo variables a factores para poder hacer el anova
rend$ubicacion<-as.factor(rend$ubicacion)
levels(rend$especie)<-c("A","B","C")

#tablas de valores medios, desvios, suma y cantidad de observaciones
tapply(rend$diametro,list(rend$especie,rend$ubicacion),
       mean,na.rm=TRUE) #medias

tapply(rend$diametro,list(rend$especie,rend$ubicacion),
       function(x) sum(!is.na(x))) #observaciones por nivel

tapply(rend$diametro,list(rend$especie,rend$ubicacion),
       sd) #desvios

#Diagramas de cajas
par(mfrow=c(1,2))
boxplot(rend$diametro~rend$especie)
boxplot(rend$diametro~rend$ubicacion)
interaction.plot(rend$especie,rend$ubicacion,rend$diametro)
interaction.plot(rend$ubicacion,rend$especie,rend$diametro)
interaction2wt(diametro ~ especie+ubicacion, data=rend)          # library(HH)

#ANOVA
#utilizando funci?n 'aov'
#estima los coeficientes quitando el primer nivel de cada factor (por defecto)
Anova1=aov(diametro ~ ubicacion+especie, data=rend)
summary(Anova1)
coefficients(Anova1)
model.matrix(Anova1)

#utilizando funci?n 'lm'
reg=lm(diametro ~ ubicacion+especie, data=rend)
summary(reg)
anova(reg)
model.matrix(reg)

#estima los coeficientes contrastando con el nivel base (en este caso, por defecto el nivel 1)
Anova2=aov(diametro ~ ubicacion+especie, data=rend, contrasts=list(ubicacion=contr.treatment, especie=contr.treatment), na.action = na.exclude)
summary(Anova2)
coefficients(Anova2)
model.matrix(Anova2)

#estima los coeficientes anulando el ultimo nivel de 'ubicacion' y
# anulando la suma de las especies
Anova3=aov(diametro ~ ubicacion+especie, data=rend, contrasts = list(ubicacion = contr.SAS, especie = contr.sum), na.action =na.exclude)
Anova3
summary(Anova3)
model.matrix(Anova3)

#Vemos coefientes estimados en cada anova
round(Anova1$coef,10)
round(Anova2$coef,10)
round(Anova3$coef,10)

# si quisiera cambiar el nivel de base ser?a as?
Anova4=lm(diametro ~ C(ubicacion,contr.treatment,base=4)+C(especie,contr.treatment,base=3), data=rend)
summary(Anova4)
model.matrix(Anova4)
round(coefficients(Anova4),10)


#ESTADISTICO F

#F ubicacion
cmt=summary(Anova3)[[1]][1,2]/summary(Anova3)[[1]][1,1]   #Cuadrados medios ubicacion
cmr=summary(Anova3)[[1]][3,2]/summary(Anova3)[[1]][3,1]   #Cuadrados medios errores
f.trat=cmt/cmr
f.trat
qf(0.95,3,54)      # valor cr?tico
1-pf(f.trat,3,54)  # p-valor

#F especie
cmb=summary(Anova3)[[1]][2,2]/summary(Anova3)[[1]][2,1]     #Cuadrados medios especie
f.bloq=cmb/cmr
f.bloq
qf(0.95,2,54)      # valor cr?tico
1-pf(f.bloq,2,54)  # p-valor

# F (ubicacion + especie)
scexplicada=sum(summary(Anova3)[[1]][1:2,2])  #suma de cuadrados modelo
cmmodelo=scexplicada/sum(summary(Anova3)[[1]][1:2,1])   #cuadrados medios modelo
f=cmmodelo/cmr
f
1-pf(f,5,54)

# comparaciones m?ltiples (library(multcomp))
aa=glht(Anova1, linfct = mcp(ubicacion= "Tukey"))
summary(aa)
confint(aa)
plot(aa,main="Ubicacion")

bb=glht(Anova1, linfct = mcp(especie= "Tukey"))
summary(bb)
confint(bb)
plot(bb,main="Especies")

# intervalos de confianza Tukey/Scheffe/Bonferroni (ubicacion 2 - ubicacion 3)

# Tukey
dif<-abs(diff(Anova1$coefficients[2:3]))      # diferencia entre ambos tratamientos
mse<-summary(Anova1)[[1]][3,3]           # estimaci?n de s2
q1<-qtukey(0.95,nmeans=3,df=48)/sqrt(2)  # nmeans es el n?mero de grupos y df = total de datos - nmeans
sdif<-sqrt(mse*(1/3+1/3))
CI95t<-c(dif-q1*sdif,dif+q1*sdif)
names(CI95t)<-c('linf','lsup')

# Scheff?e
f<-sqrt((3-1)*qf(0.95,3-1,48-3))
CI95f<-c(dif-f*sdif,dif+f*sdif)
names(CI95f)<-c('linf','lsup')

# Bonferroni
b<-qt(1-0.05/choose(4,2),48-6)
CI95b<-c(dif-b*sdif,dif+b*sdif)
names(CI95b)<-c('linf','lsup')

ci<-rbind(CI95t,CI95f,CI95b)
rownames(ci)<-c('Tukey','Schefee','Bonferroni')
ci

# Análisis Residuos
par(mfrow=c(1,2))
plot(fitted(Anova1),residuals(Anova1),xlab="y_estimados",ylab="Residuos")
abline(h=0,col="red")

qqnorm(residuals(Anova1)) ; qqline(residuals(Anova1))
install.packages("tseries")
library(tseries)
jarque.bera.test(residuals(Anova1)) #No rechazamos Normalidad
shapiro.test(residuals(Anova1)) #Rechazamos Normalidad

# Analizamos homogeneidad de varianzas
library(car)
leveneTest(diametro~especie*ubicacion,data=rend) #No rechazamos la hipotesis de homogeneidad de varianzas

library(stats)
bartlett.test(rend$diametro,paste(rend$especie,rend$ubicacion,sep="")) #No rechazamos la hipotesis de homogeneidad de varianzas

############################
########  ANOVA  ###########
############################

rm(list=ls())

########################
#### ANOVA: UNA VIA ####
########################

install.packages("faraway")
library(faraway)
data(coagulation)
?coagulation #Tiempo de coagulacion en la sangre para 
              #24 animales a los que se les asignaron 4 tipo de dietas
summary(coagulation)


## Gr?ficamos los datos
?stripchart

par(mfrow=c(1,2))
plot(coag ~ diet, data=coagulation, xlab="Dieta", ylab="Tiempo de coagulaci?n", col="red")
with(coagulation, stripchart(coag ~ diet, vertical=TRUE, method="stack", xlab="Dieta", ylab="Tiempo de coagulaci?n", col="darkred"))

# Media y desv?o de coagulaci?n seg?n la dieta
tabla1<-aggregate(coagulation$coag, by=list(coagulation$diet), mean)
tabla1
mean(coagulation$coag)
tabla2<-aggregate(coagulation$coag, by=list(coagulation$diet), sd)
tabla2


# Tres formas de ajustar el mismo modelo: distintas restricciones
# (1) estima los coeficientes anulando el primer nivel

modelo1 <- lm(coag ~ diet, coagulation)
summary(modelo1)
model.matrix(modelo1)

# Pueba F
screg<-(sum((modelo1$fitted.values-mean(coagulation$coag))^2))
scerr<-sum(modelo1$residuals^2)
k=3
n=nrow(coagulation)
test_F<-(screg/k)/(scerr/(n-k-1))
test_F

# Con la funci?n aov (estima los coeficientes anulando el primer nivel)
?aov
modelo1.1<-aov(coag ~ diet, coagulation)
summary(modelo1.1)
names(modelo1.1)
modelo1.1$coefficients


# (2) sin t?rmino independiente

modelo2<- lm(coag ~ diet -1, coagulation)
summary(modelo2)

modelo2.1<-aov(coag ~ diet -1, coagulation)
summary(modelo2.1)

# El R^2 no est? calculado correctamente dada la ausencia del t?rmino independiente
# El test F tiene como hip?tesis nula que la esperanza de y es cero (no es un test interesante)
# Para generar el test de que las medias de los niveles son iguales ajustamos el modelo nulo y comparamos
# con la tabla anova

modelonulo<-lm(coag ~ 1, coagulation)
summary(modelonulo)

anova(modelonulo,modelo2) # se obtiene el mismo estad?stico F del modelo1

# Comparaciones de a parejas

# IC al 95% para C-B
n<-24
I<-4
Ji<-nrow(coagulation[coagulation$diet=="C",])
Jj<-nrow(coagulation[coagulation$diet=="B",])
aimaj<-modelo1$coefficients[3]-modelo1$coefficients[2]
se<-(2.366)*(sqrt((1/Ji)+(1/Jj)))
tval<-qt(0.975, 20)

IC<-c(aimaj-tval*se, aimaj + tval*se)
IC

# Todos los intervalos
# Tukey HSD : honest significant difference
?TukeyHSD
ic<-TukeyHSD(aov(coag ~ diet, coagulation))
ic
plot(ic)

# para uno solo (B-A)
# valor cr?tico
qtuk<-qtukey(0.95,4,20)/sqrt(2)
ICab<-c(aimaj-qtuk*se, aimaj+qtuk*se)
ICab

library(gplots)
plotmeans(coag ~ diet, xlab="Dieta", ylab="Coagulaci?n",
          main="Media con IC 95%", data=coagulation)

# Diagnostico Modelo (1)
par(mfrow=c(1,2))
plot(fitted(modelo1),residuals(modelo1),xlab="y_estimados",ylab="Residuos")
abline(h=0,col="red")

qqnorm(residuals(modelo1)) ; qqline(residuals(modelo1))
jarque.bera.test(residuals(modelo1)) #No rechazamos Normalidad
shapiro.test(residuals(modelo1)) #Rechazamos Normalidad

# Analizamos homogeneidad de varianzas
library(car)
leveneTest(coag~diet,data=coagulation) #No rechazamos la hipotesis de homogeneidad de varianzas

library(stats)
bartlett.test(coagulation$coag,coagulation$diet) #No rechazamos la hipotesis de homogeneidad de varianzas



##****************************
####### ANOVA: DOS V?AS  #####
##****************************

library(faraway)
data(pvc)
?pvc #Produccion de plastico PVC 3 operadores utilizando 8 diferentes tipos de resina
str(pvc)
summary(pvc)
table(pvc$resin, pvc$operator)

par(mfrow=c(1,2))
stripchart(pvc$psize ~ pvc$resin, xlab="Tamaño de la particula", ylab="Resina de autovia")
stripchart(pvc$psize ~ pvc$operator, xlab="Tamaño de la particula", ylab="Operador")

#Observo la interacción
interaction2wt(psize~resin+operator,data=pvc)

# Modelo completo
modelo5<-lm(psize ~ operator*resin, data=pvc)
anova(modelo5) # para chequear a si la interaccion es significativa

modelo5.1<-aov(psize ~ operator*resin, data=pvc)
summary(modelo5.1)

summary(modelo5)

modelo5.2<-lm(psize ~ operator+resin, data=pvc)
summary(modelo5.2)

modelo5.3<-aov(psize ~ operator+resin, data=pvc)
summary(modelo5.3)

# comparaciones m?ltiples (library(multcomp))
aa=glht(modelo5.2, linfct = mcp(operator= "Tukey"))
summary(aa)
confint(aa)
plot(aa,main="Operador")

bb=glht(modelo5.2, linfct = mcp(resin= "Tukey"))
summary(bb)
confint(bb)
plot(bb,main="Resin")

# Análisis Residuos
par(mfrow=c(1,2))
plot(fitted(modelo5.2),residuals(modelo5.2),xlab="y_estimados",ylab="Residuos")
abline(h=0,col="red")

qqnorm(residuals(modelo5.2)) ; qqline(residuals(modelo5.2))
library(tseries)
jarque.bera.test(residuals(modelo5.2)) #Rechazamos Normalidad
shapiro.test(residuals(modelo5.2)) #Rechazamos Normalidad

# Analizamos homogeneidad de varianzas
library(car)
leveneTest(psize~operator,data=pvc) #No rechazamos la hipotesis de homogeneidad de varianzas
leveneTest(psize~resin,data=pvc)
library(stats)
bartlett.test(pvc$psize,paste(pvc$operator,pvc$resin,sep="")) #No rechazamos la hipotesis de homogeneidad de varianzas

#Analisis outliers

outlierTest(modelo5.2)
pvc[c(45,46),]
plot(as.numeric(paste(pvc$resin,pvc$operator,sep="")),pvc$psize,col=as.numeric(paste(pvc$resin,pvc$operator,sep="")),pch=19,cex=1.2)
abline(v=as.numeric(paste(pvc$resin,pvc$operator,sep="")))

modelosinAO<-lm(psize ~ operator+resin, data=pvc[-45,])
summary(modelosinAO)
anova(modelosinAO)
qqnorm(residuals(modelosinAO)) ; qqline(residuals(modelosinAO))
library(tseries)
jarque.bera.test(residuals(modelosinAO)) #Ahora no rechazamos Normalidad
shapiro.test(residuals(modelosinAO))  #Ahora no rechazamos Normalidad
