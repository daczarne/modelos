# pr�ctico mi�rcoles 24 de mayo 2007

# covariables cualitativas y ANOVA

ruta="C:/Users/dacza/Dropbox/UdelaR/CCEEA/Semestre 9/Modelos Lineales/Talleres de R/Pr�cticos 2017"
load(file=paste0(ruta,"/practico24_5.RData"))

head(datos)
summary(datos)

class(datos$sexo)
class(datos$genotipo)
class(datos$actividad)

# regresamos actividad seg�n el sexo
m1<-lm(actividad~sexo,data=datos)
summary(m1)

# regresamos actividad seg�n el sexo sin constante
m2<-lm(actividad~sexo-1,data=datos)
summary(m2)

aggregate(datos$actividad,data.frame(sexo=datos$sexo),mean)

# que pasa si en el modelo "m1" queremos cambiar la categor�a de referencia?
datos$sexo<-relevel(datos$sexo,ref="masculino")
m1<-lm(actividad~sexo,data=datos)
summary(m1)

# o hagamos las cosas "a mano"
datos$fem<-ifelse(datos$sexo=="femenino",1,0)
m3<-lm(actividad~fem,data=datos)
summary(m3)


# regresmos ahora la actividad sobre el genotipo

m4<-lm(actividad~genotipo,data=datos)
summary(m4)
# usando la funci�n relevel se puede cambiar la categor�a de referencia

# o mas claro a�n, hag�moslo sin constante
m5<-lm(actividad~genotipo-1,data=datos)
summary(m5)

# que es lo mismo que...
aggregate(datos$actividad,data.frame(genotipo=datos$genotipo),mean)

# el R crea una indicatriz para cada nivel de genotipo
datos$fs<-ifelse(datos$genotipo=="fs",1,0)
datos$ss<-ifelse(datos$genotipo=="ss",1,0)
m6<-lm(actividad~fs+ss,data=datos)
summary(m6)

# y como incluir las dos variables en el modelo?
mod1<-lm(actividad~sexo+genotipo,data=datos)
summary(mod1)

# lo importante es la intgerpretaci�n de los coeficientes!!!!

# y tambi�n podr�amos incluir un t�rmino de interacci�n
summary(update(mod1,.~.+sexo:genotipo))


# qu� es ANOVA?
anova(m1)
anova(m4)
anova(mod1)