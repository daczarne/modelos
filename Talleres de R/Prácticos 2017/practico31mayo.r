# práctico miércoles 31 de mayo - Ridge

library(faraway)
dat(seatpos)

head(seatpos)

# hagamos un modelo de regresión múltiple

m1<-lm(hipcenter~.,data=seatpos)
summary(m1)

# ahí hay algo raro

library(car)
vif(m1)

# sacamos Ht
m2<-update(m1,.~.-Ht)
summary(m2)

# vamos de nuevo con el VIF
vif(m2)

# sacamos Htshoes
m3<-update(m2,.~.-HtShoes)
summary(m3)

# y podríamos seguir sacando...
# algo que convendría haber hecho es (o era):
round(cor(seatpos),3)

# probemos con ridge
library(MASS)

lambda<-seq(0,100,0.1)
r1<-lm.ridge(hipcenter~.,data=seatpos,lambda=lambda)

plot(lambda,r1$GCV,type='l',lwd=2,col='navyblue',ylab='error de validación cruzada')
cual<-which.min(r1$GCV)
abline(v=lambda[cual],lty=2)
abline(h=r1$GCV[cual],lty=2)
text(80, 44, substitute(lambda == x,list(x=lambda[cual])))
text(80, 43, substitute(GCV == x,list(x=r1$GCV[cual])))

# también se puede usar la función select
select(r1)

plot(r1) # ojo! los coeficientes corresponden a los datos "escalados"
legend('bottomright',names(seatpos)[-9],col=1:6,lty=1:5,cex=.8,bty='n')
abline(h=0)
abline(v=lambda[cual],lty=2)

# podemos ver los coeficientes en su versión original mediante el argumento "scales"
matplot(lambda,
        t(apply(t(r1$coef),1,function(x){return(x/r1$scales)})),
        type="l",xlab="lambda",ylab="betas en escala original",lwd=2)
legend('bottomright',names(seatpos)[-9],col=1:6,lty=1:5,cex=.8,bty='n',lwd=2)
abline(h=0)
abline(v=lambda[cual],lty=2)

