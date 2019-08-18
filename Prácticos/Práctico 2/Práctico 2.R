####################
#### PRÁCTICO 2 ####
####################

library(pdftools)

#####################
#### EJERCICIO 5 ####
#####################

rm(list = ls())

# Genero los datos
y=c(8,12,21,31,39,58)
x=c(0,10,20,30,40,60)
n=length(x)

# Calculo beta 1 gorro
Sxy=cov(x,y)
Sx=var(x)
beta1=Sxy/Sx

# Calculo el desvío de beta 1 gorro
X=cbind(rep(1,6),x)
XtX=t(X)%*%X
XtXinv=solve(XtX)

beta0=mean(y)-beta1*mean(x)

yhat=NULL
for(i in 1:length(y)){
      yhat[i]=beta0 + beta1*x[i]
}
uhat=y-yhat
sigmahat=t(uhat)%*%uhat / (n-2)

vb0=sigmahat * XtXinv[1,1]
vb1=sigmahat * XtXinv[2,2]

sb0=sqrt(vb0)
sb1=sqrt(vb1)

# Construyo el intervalo
t=qt((1-0.05/2), 4, lower.tail=TRUE)

liminf=beta1-t*sb1
limsup=beta1+t*sb1

# Verificación
summary(lm(y ~ x))

#####################
#### EJERCICIO 6 ####
#####################

rm(list = ls())

# Genero los datos
y=c(60,61,62,63,65,67,68,69,70,72,71,70)
x=c(163,163,165,166,168,169,170,170,171,172,172,174)
n=length(x)

# Estimo los beta
X=cbind(rep(1,n),x)
beta=solve(t(X) %*% X) %*% (t(X) %*% y)

# Estimo sigma
yhat=NULL
for(i in 1:length(y)){
      yhat[i]=beta[1] + beta[2]*x[i]
}
uhat=y-yhat
sigma2hat=as.numeric(t(uhat)%*%uhat / (n-2))
sigmahat=sqrt(sigma2hat)

# Estimo las varianzas de los beta
s2beta=sigma2hat * solve(t(X) %*% X)
s2beta1=s2beta[2,2]

e=beta[2]/sqrt(s2beta1)
t=qt((1-0.05/2), df=n-2, lower.tail=TRUE)

if(abs(e) > t){
      print("Rechazo H0 - No existe evidencia de que el par?metro no sea significativo")
}else{
      print("No Rechazo H0 - Existe evidencia de que el par?metro no es significativo")
}

# Verificación
summary(lm(y ~ x))

#####################
#### EJERCICIO 7 ####
#####################

rm(list = ls())

# Genero los datos
x=c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
y=c(1,5,4,7,10,8,9,13,14,13,18)
n=length(y)
alpha=0.05

#### Parte a ####

# Estimo los beta
X = cbind(rep(1,n),x)
beta = solve(t(X) %*% X) %*% (t(X) %*% y)

# Estimo sigma
yhat = NULL
for(i in 1:length(y)){
      yhat[i] = beta[1] + beta[2]*x[i]
}
uhat = y - yhat
sigma2hat = as.numeric(t(uhat)%*%uhat / (n-dim(beta)[1]))
sigmahat = sqrt(sigma2hat)

# Estimo las varianzas de los beta
s2beta = sigma2hat * solve(t(X) %*% X)

#### Parte b ####
s2beta2 = s2beta[2,2]
e = beta[2]/sqrt(s2beta2)
t = qt((1-alpha/2), df=n-2, lower.tail=TRUE)

if(abs(e) > t){
      print("Rechazo H0 - No existe evidencia de que el par?metro no sea significativo")
}else{
      print("No Rechazo H0 - Existe evidencia de que el par?metro no es significativo")
}

#### Parte c ####
x0 = c(1,3)
E_y_3 = x0 %*% beta

(liminf_c = E_y_3 - t * sqrt(sigma2hat * t(x0) %*% solve(t(X)%*%X) %*% x0))
(limsup_c = E_y_3 + t * sqrt(sigma2hat * t(x0) %*% solve(t(X)%*%X) %*% x0))

#### Parte c ####
y_3 = x0 %*% beta

(liminf_d = y_3 - t * sqrt(sigma2hat * (1+t(x0) %*% solve(t(X)%*%X) %*% x0)))
(limsup_d = y_3 + t * sqrt(sigma2hat * (1+t(x0) %*% solve(t(X)%*%X) %*% x0)))

#### Verificación ####
summary(lm(y ~ x))

#$###################
#### EJERCICIO 8 ####
#####################

rm(list = ls())

# txt <- pdf_text("Pr?ctico 2.pdf")
# cat(txt[2])

# Cargo los datos
data <- read.csv("data8.csv", header=T)
n = dim(data)[1]
alpha = 0.05

#### Parte a ####
beta = summary(lm(Velocidad ~ Densidad, data=data))$coefficients[,1]

#### Parte b ####
uhat = summary(lm(Velocidad ~ Densidad, data=data))$residual
sigma2 = as.numeric(t(uhat) %*% uhat / (n - length(beta)))
sdbeta = summary(lm(Velocidad ~ Densidad, data=data))$coefficient[,2]

#### Parte c ####
liminfb1 = beta[1] - qt((1-alpha/2), df=n-length(beta)) * sdbeta[1]
limsupb1 = beta[1] + qt((1-alpha/2), df=n-length(beta)) * sdbeta[1]

liminfb2 = beta[2] - qt((1-alpha/2), df=n-length(beta)) * sdbeta[2]
limsupb2 = beta[2] + qt((1-alpha/2), df=n-length(beta)) * sdbeta[2]

#### Parte d ####
anova(lm(Velocidad ~ Densidad, data=data))
qf((1-alpha), df1=1, df2=n-2)
qt((1-alpha/2), df=n-2)^2

#### Parte e ####
x0 = c(1,50)
E_y_50 = x0 %*% beta

X = cbind(rep(1,n), data$Densidad)
t = qt((1-.1/2), df=n-2)

(liminf_e = E_y_50 - t * sqrt(sigma2 * t(x0) %*% solve(t(X)%*%X) %*% x0))
(limsup_e = E_y_50 + t * sqrt(sigma2 * t(x0) %*% solve(t(X)%*%X) %*% x0))

#### Parte f ####
beta = summary(lm(sqrt(Velocidad) ~ Densidad, data=data))$coefficients[,1]

sigma2 = summary(lm(sqrt(Velocidad) ~ Densidad, data=data))[[6]]^2
sdbeta = summary(lm(sqrt(Velocidad) ~ Densidad, data=data))$coefficient[,2]

liminfb1 = beta[1]-qt((1-alpha/2), df=n-length(beta))*sdbeta[1]
limsupb1 = beta[1]+qt((1-alpha/2), df=n-length(beta))*sdbeta[1]

liminfb2 = beta[2]-qt((1-alpha/2), df=n-length(beta))*sdbeta[2]
limsupb2 = beta[2]+qt((1-alpha/2), df=n-length(beta))*sdbeta[2]

anova(lm(sqrt(Velocidad) ~ Densidad, data=data))
qf((1-alpha), df1=1, df2=n-2)
qt((1-alpha/2), df=n-2)^2

x0 = c(1,sqrt(50))
E_y_50 = x0 %*% beta

X = cbind(rep(1,n), sqrt(data$Densidad))

liminf_f = E_y_50 - t * sqrt(sigma2 * t(x0) %*% solve(t(X)%*%X) %*% x0)
limsup_f = E_y_50 + t * sqrt(sigma2 * t(x0) %*% solve(t(X)%*%X) %*% x0)

################################
#### FIN DE LA PROGRAMACIÓN ####
################################