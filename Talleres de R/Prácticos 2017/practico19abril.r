# práctico miércoles 5 de abril

# ejercicio 2 - práctico 2

# intervalos de confianza de beta_0, beta_1 y sigma2
###################################
#     parámetros y simulación     #
###################################
n      <- 100
beta_0 <- 4
beta_1 <- 1.7
sigma2 <- 2
x      <- rnorm(n,10,1)
# x<-x-mean(x)  # ver esto al final
y <- beta_0 + beta_1*x + rnorm(n,0,sqrt(sigma2))



####################################
#     estimación de paràmetros     #
####################################

Y <- matrix(y,ncol=1)
X <- cbind(1,x)
beta_g <- solve(t(X)%*%X)%*%t(X)%*%Y

# estimación de sigma2
err      <- y - X%*%beta_g
sigma2_g <- sum(err^2)/(n-2)



#######################################################
#     INTERVALO DE CONFIANZA PARA beta_0 y beta_1     #
#######################################################

alfa <- 0.05

# matriz de covarianzas de las estimaciones de beta_0 y beta_1
cov_b <- sigma2_g*solve(t(X)%*%X)

delta <- qt(1-alfa/2,n-2)*sqrt(diag(cov_b))
ci<-data.frame(linf=beta_g-delta, esti=beta_g, lsup=beta_g+delta)

# a ver si hicimos bien...
confint(lm(y~x))

##############################################
#     INTERVALO DE CONFIANZA PARA sigma2     #
##############################################

li <- (n-2)*sigma2_g/qchisq(1-alfa/2,n-2)
ls <- (n-2)*sigma2_g/qchisq(alfa/2,n-2)
ci <- rbind(ci,c(li,sigma2_g,ls))

rownames(ci)<-c('beta_0','beta_1','sigma2')

c(beta_0,beta_1,sigma2)

#####################################################
#     INTERVALO DE CONFIANZA PARA la predicción     #
#####################################################

plot(x,y,pch=16,col='red',xlab='variable explicativa',ylab='variable a explicar')

# añadimos la recta de regresión
mod1<-lm(y~x)
betas<-coef(mod1)
abline(betas,lwd=2)

# predicción en x = 11
y11<-betas[1]+betas[2]*11
points(11,y11,pch=1,cex=2,col='red')

# varianza de la E(Y|x=11)
x0<-matrix(c(1,11),nrow=1,ncol=2)
var_y11<-x0%*%vcov(mod1)%*%t(x0)
desvio<-sqrt(var_y11)

# intervalo para E(Y|x=11)
alfa<-0.05
segments(11,y11+qt(1-alfa/2,n-2)*desvio,11,y11-qt(1-alfa/2,n-2)*desvio,11,col='blue',lwd=2)

# y si quisiéramos hacer una banda de confianza????
equis<-seq(min(x),max(x),by=0.01)
pred<-predict(mod1,newdata=data.frame(x=equis),interval='confidence')

polygon(c(equis,rev(equis)),c(pred[,2],rev(pred[,3])),col=rgb(0,0,1,0.5),border=NA)

# y esto otro que será???
pred2<-predict(mod1,newdata=data.frame(x=equis),interval='prediction')
polygon(c(equis,rev(equis)),c(pred2[,2],rev(pred2[,3])),col=rgb(0,1,0,0.25),border=NA)

# ese esel intervalo para Y|X=11

#########################
#	RESUMEN DEL MODELO	#
#########################

summary(mod1)

# con otro ejemplo
data(iris)

m1<-lm(Petal.Width~Petal.Length+Sepal.Width, data=iris)
summary(m1)

SCY<-sum((iris$Petal.Width-mean(iris$Petal.Width))^2)
SCres<-sum(residuals(m1)^2)

1-SCres/SCY
(SCY-SCres)/(SCY)   # donde SCY-SCres es...???
