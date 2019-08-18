####################
#### PRÁCTICO 4 ####
####################

#$###################
#### EJERCICIO 4 ####
#$###################

x1=c(1,2,3,4)
x2=x1^2
n=length(x1)

X=cbind(rep(1,n), x1, x2)

XtX=t(X) %*% X
XtXinv=solve(XtX)

#$###################
#### EJERCICIO 5 ####
#$###################

rm(list=ls())

x1=c(1,2,-1,2)
x2=c(1,1,1,-1)
y=c(6.6,7.8,2.1,0.4)

alpha=0.05
n=length(x1)
q=1

X <- cbind(x1,x2)

beta1 <- summary(lm(y~X -1))$coefficients[1]
beta2 <- summary(lm(y~X -1))$coefficients[2]
sigma2 <- summary(lm(y~X -1))$sigma^2

numerador <- c(-2,1) %*% solve(t(X)%*%X) %*% c(-2,1) / q
F. = qf(1-alpha/2, q, n-2)

F0 = (1/numerador) * (-2*beta1 + beta2)^2 * (1/sigma2)

################################
#### FIN DE LA PROGRAMACIÓN ####
################################