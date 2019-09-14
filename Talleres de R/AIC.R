datos<-read.csv("nyc.csv")
head(datos)
datos=datos[,-c(1,2)] #Sacamos las variables innecesarias
head(datos)

datos$Service2<-datos$Service*10+rnorm(168,0,1)
datos$Food2<-datos$Food*10+rnorm(168,0,1)
head(datos) #Agregue 2 variables mas que son una cuasi combinacion lineal de las ya existentes

#Veremos el R2, R2Ajustado y el AIC de distintos modelos

reguno<-lm(Price~Food,data=datos) #Con una sola variable explicativa
regunoR2<-summary(reguno)$r.squared
regunoR2A<-summary(reguno)$adj.r.squared
regunoAIC<-AIC(reguno)
resumen<-data.frame(k=1,R2=regunoR2,R2A=regunoR2A,AIC=regunoAIC)

regdos<-lm(Price~Food+Service,data=datos) #Con dos variables explicativas
regdosR2<-summary(regdos)$r.squared
regdosR2A<-summary(regdos)$adj.r.squared
regdosAIC<-AIC(regdos)
resumen[2,]<-c(2,regdosR2,regdosR2A,regdosAIC)

regtres<-lm(Price~Food+Service+Food2,data=datos) #Con tres variables explicativas
regtresR2<-summary(regtres)$r.squared
regtresR2A<-summary(regtres)$adj.r.squared
regtresAIC<-AIC(regtres)
resumen[3,]<-c(3,regtresR2,regtresR2A,regtresAIC)

regcuatro<-lm(Price~Food+Service+Food2+Service2,data=datos)#Con cuatro variables explicativas
regcuatroR2<-summary(regcuatro)$r.squared
regcuatroR2A<-summary(regcuatro)$adj.r.squared
regcuatroAIC<-AIC(regcuatro)
resumen[4,]<-c(4,regcuatroR2,regcuatroR2A,regcuatroAIC)

regtodos<-lm(Price~.,data=datos) #Con todas las variables explicativas que se encuentran en la base
regtodosR2<-summary(regtodos)$r.squared
regtodosR2A<-summary(regtodos)$adj.r.squared
regtodosAIC<-AIC(regtodos)
resumen[5,]<-c(7,regtodosR2,regtodosR2A,regtodosAIC)

regtodos2<-lm(Price~.-Service2-Food2,data=datos) #Con todas las variables explicativas que se encuentran en la base
regtodos2R2<-summary(regtodos)$r.squared
regtodos2R2A<-summary(regtodos)$adj.r.squared
regtodos2AIC<-AIC(regtodos)
resumen[6,]<-c(5,regtodosR2,regtodosR2A,regtodosAIC)
