#$##################
#### PR?CTICO 1 ####
#$##################

library(tidyverse)
library(MASS)
library(pdftools)
library(gridExtra)


# Multiple plot function

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.

# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   
   numPlots = length(plots)
   
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
   }
   
   if (numPlots==1) {
      print(plots[[1]])
      
   } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
         # Get the i,j matrix positions of the regions that contain this subplot
         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
         
         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
      }
   }
}

#$###################
#### EJERCICIO 8 ####
#$###################

y = c(77.4, 76.7, 78.2, 84.1, 84.5, 83.7, 88.9, 89.2, 89.7, 94.8, 94.7, 95.9)
x = as.factor(c(rep(150,3), rep(200,3), rep(250,3), rep(300,3)))
datos = data.frame(y,x)

resumen = datos %>% group_by(x) %>% summarize(obs=n(), gmean=mean(y)) %>% print()
sizes = resumen[[2]]
medias = resumen[[3]]

yhat = lm(y ~ x, data=datos)$fitted

scrlof = NULL

for(i in sizes){
      for(j in 1:sizes[i]){
            scrlof[]
      }
}

sizes[1] * (medias[1] - yhat[1])^2
length(yhat)
length(medias)


#$########################
#### EJERCICIO EN R 1 ####
#$########################

data("cars")

#### SE PIDE a ####

ggplot(cars, aes(speed, dist)) + 
      geom_point(color="steelblue", size=3, alpha=1/2) +
      ggtitle(label="Cars data set") + labs(x="Speed", y="Distance") + 
      theme(plot.title=element_text(hjust=0.5))

xbar = mean(cars$speed)
ybar = mean(cars$dist)

xcent = NULL
for(i in 1:length(cars$speed)){xcent[i] = cars$speed[i] - xbar}

ycent = NULL
for(i in 1:length(cars$dist)){ycent[i] = cars$dist[i] - ybar}

beta1 = sum(xcent * ycent) / sum(xcent^2)
beta0 = ybar - beta1 * xbar

lm(dist ~ speed, data=cars)$coef #Verificaci?n

#### SE PIDE b ####

X = cbind(rep(1, times=length(cars$speed)), cars$speed)
Y = as.matrix(cars$dist)

beta_hat = solve(t(X) %*% X) %*% t(X) %*% Y

#### SE PIDE c ####

summary(lm(dist ~ speed, data=cars))
summary(lm(log(dist) ~ speed, data=cars))
summary(lm(dist ~ log(speed), data=cars))
summary(lm(log(dist) ~ log(speed), data=cars))
summary(lm(dist ~ speed + speed^2, data=cars))
summary(lm(dist ~ speed + speed^3, data=cars))
summary(lm(log(dist) ~ speed + speed^2, data=cars))
summary(lm(dist ~ speed^2, data=cars))

# Mejor ajuste (en base al R^2 ajustado) es con el modelo log-log


#$########################
#### EJERCICIO EN R 2 ####
#$########################

#### SE PIDE a ####

n = 1000
rho <- 0.8
mu1 <- 1; s1 <- 1
mu2 <- 2; s2 <- 1

mu <- c(mu1,mu2) 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)

set.seed(1234)
data <- mvrnorm(n, mu=mu, Sigma=sigma)
colnames(data) <- c("X","Y")
data <- as.data.frame(data)

ggplot(data, aes(X, Y)) + 
      geom_point(color="steelblue", size=3, alpha=1/2) +
      ggtitle(label="Simulation") + labs(x="X", y="Y") + 
      theme(plot.title=element_text(hjust=0.5))

#### SE PIDE b ####

reg <- summary(lm(Y ~ X, as.data.frame(data)))

ggplot(data, aes(X, Y)) + 
      geom_point(color="steelblue", size=3, alpha=1/2) +
      ggtitle(label="Simulation") + labs(x="X", y="Y") + 
      theme(plot.title=element_text(hjust=0.5)) + 
      geom_smooth(method="lm", se=FALSE, col="red")

#### SE PIDE c ####

ybar = mean(data$Y)

ybar2 = NULL

beta0 = reg$coefficients[1,1]
beta1 = reg$coefficients[2,1]


#$########################
#### EJERCICIO EN R 3 ####
#$########################

# Leo el pdf con R para extraer la base y pegarla en un txt
txt <- pdf_text("Pr?ctico 1.pdf")
cat(txt[3])

# Leo el txt
data <- read.table("data.txt", header=TRUE, row.names=1)

r1 <- lm(y.a. ~ x.a.d., data=data)
r2 <- lm(y.b. ~ x.a.d., data=data)
r3 <- lm(y.c. ~ x.a.d., data=data)
r4 <- lm(y.d. ~ x.a.d., data=data)
r5 <- lm(y.e. ~ x.e., data=data)

p1 <- ggplot(data, aes(x.a.d., y.a.)) + 
            geom_point(color="steelblue", size=3, alpha=1/2) +
            labs(x="", y="y.a.") + 
            geom_smooth(method="lm", se=FALSE, col="red") + 
            labs(title=expression(bold("M1"))) +
            theme(plot.title=element_text(hjust=0.5))

p2 <- ggplot(data, aes(x.a.d., y.b.)) + 
            geom_point(color="steelblue", size=3, alpha=1/2) +
            labs(x="x.a.d.", y="y.b.") + 
            geom_smooth(method="lm", se=FALSE, col="red") + 
            labs(title=expression(bold("M2"))) +
            theme(plot.title=element_text(hjust=0.5))

p3 <- ggplot(data, aes(x.a.d., y.c.)) + 
            geom_point(color="steelblue", size=3, alpha=1/2) +
            labs(x="", y="y.c.") + 
            geom_smooth(method="lm", se=FALSE, col="red") +
            labs(title=expression(bold("M3"))) +
            theme(plot.title=element_text(hjust=0.5))

p4 <- ggplot(data, aes(x.a.d., y.d.)) + 
            geom_point(color="steelblue", size=3, alpha=1/2) +
            labs(x="x.a.d.", y="y.d.") + 
            geom_smooth(method="lm", se=FALSE, col="red") + 
            labs(title=expression(bold("M4"))) +
            theme(plot.title=element_text(hjust=0.5))

p5 <- ggplot(data, aes(x.e., y.e.)) + 
            geom_point(color="steelblue", size=3, alpha=1/2) +
            labs(x="x.e.", y="y.e.") + 
            geom_smooth(method="lm", se=FALSE, col="red") + 
            labs(title=expression(bold("M5"))) +
            theme(plot.title=element_text(hjust=0.5))

modelos <- list(summary(r1), summary(r2), summary(r3), summary(r4), summary(r5))

R2 <- NULL
for(i in 1:5){R2[i] <- round(modelos[[i]][[8]], 6)}
names(R2) <- c("M1", "M2", "M3", "M4", "M5")

R2a <- NULL
for(i in 1:5){R2a[i] <- round(modelos[[i]][[9]], 6)}
names(R2a) <- names(R2)

tabla <- cbind(R2, R2a)

tplot <- qplot(1:2, 1:2, geom="blank") + 
            theme(panel.background=element_blank()) + 
            theme(line=element_blank(), text=element_blank()) +
            annotation_custom(grob=tableGrob(tabla)) 

multiplot(p1, p2, p3, p4, p5, tplot, cols=3)

#$##############################
#### FIN DE LA PROGRAMACI?N ####
#$##############################