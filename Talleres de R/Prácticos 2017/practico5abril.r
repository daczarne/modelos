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
