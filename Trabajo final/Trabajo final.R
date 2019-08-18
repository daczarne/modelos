###############################
#### TRABAJO FINAL ML 2017 ####
###############################

library(dplyr)
library(ggplot2)
library(tseries) # jarque-bera's test (normality)
library(lmtest) # white's test (heteroskedasticity)
library(sampling)
library(boot) # LOOCV
library(cluster) # Clustering de regresores
library(leaps) # Best subset
library(glmnet) # Ridge and lasso

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

naci <- read.table("naci.txt",head=T,sep=",")
names(naci) <- c("educ","fuma","gest","peso")
naci$peso <- as.numeric(naci$peso)

##############################
#### Análisis descriptivo ####
##############################

sink(file="Análisis descriptivo.txt")
summary(naci)
group_by(naci, fuma) %>% 
      summarise(obs=n(), educ=round(mean(educ),2), gest=round(mean(gest),2),
                peso=round(mean(peso),2))
sink()

table(complete.cases(naci))

sink(file="correlaciones.txt")
dplyr::select(naci, peso, educ, gest) %>% cor()
sink()

p1 <- ggplot(naci, aes(x=peso, colour="red", fill="red")) + 
      geom_histogram(bins=25) +
      theme(text=element_text(family="sans"),
            legend.position="none",
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Peso del niño al nacer")
      
p2 <- ggplot(naci, aes(x=fuma, y=peso)) +
      geom_boxplot(fill=c("#2ca25f","#1F3552"), alpha=0.7, 
                   outlier.colour="black",
                   outlier.fill="black") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      coord_flip() +
      scale_x_discrete(labels=c("si"="Si","no"="No")) + 
      labs(x="Madre fumadora",y="Peso del niño al nacer")

p3 <- ggplot(naci, aes(x=educ, y=peso, colour=educ)) + 
      geom_point() + 
      theme(text=element_text(family="sans"),
            legend.position="none",
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Años de educación de la madre",y="Peso del niño al nacer")

p4 <- ggplot(naci, aes(x=gest, y=peso, colour=gest)) + 
      geom_point() + 
      theme(text=element_text(family="sans"),
            legend.position="none",
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Tiempo de gestación",y="Peso del niño al nacer")

pdf(file="Descripción de variables.pdf", width=9, height=6, title="Diagnnóstico Modelo 1")
multiplot(p1,p2,p3,p4, cols=2)
dev.off()

nofumapeso = naci %>% dplyr::filter(fuma == "no") %>% dplyr::select(peso) %>% unlist()
sifumapeso = naci %>% dplyr::filter(fuma == "si") %>% dplyr::select(peso) %>% unlist()

sink(file="Igualdad de media, fuma nofuma, peso")
t.test(x=nofumapeso, y=sifumapeso, alternative="two.sided")
sink()

pdf(file="Boxplot gest by fuma.pdf", width=9, height=6)
ggplot(naci, aes(x=fuma, y=gest)) +
      geom_boxplot(fill=c("#2ca25f","#1F3552"), alpha=0.7, 
                   outlier.colour="black",
                   outlier.fill="black") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      coord_flip() +
      scale_x_discrete(labels=c("si"="Si","no"="No")) + 
      labs(x="Madre fumadora",y="Tiempo de gestación")
dev.off()

nofumagest = naci %>% dplyr::filter(fuma == "no") %>% dplyr::select(gest) %>% unlist()
sifumagest = naci %>% dplyr::filter(fuma == "si") %>% dplyr::select(gest) %>% unlist()

sink(file="Igualdad de media, fuma nofuma, gest")
t.test(x=nofumagest, y=sifumagest, alternative="two.sided")
sink()

pdf(file="Boxplot educ by fuma.pdf", width=9, height=6)
ggplot(naci, aes(x=fuma, y=educ)) +
      geom_boxplot(fill=c("#2ca25f","#1F3552"), alpha=0.7, 
                   outlier.colour="black",
                   outlier.fill="black") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
      coord_flip() +
      scale_x_discrete(labels=c("si"="Si","no"="No")) + 
      labs(x="Madre fumadora",y="Años de educación")
dev.off()

# Multicolinealidad

modelo <- peso~.
X <- model.matrix(modelo, data=naci)[,-1]

sink(file="kappa.txt")
kappa = sqrt(max(eigen(t(X)%*%X)$values) / min(eigen(t(X)%*%X)$values))
sink()

sink(file="FIV.txt")
diag(solve(cor(X)))
sink()

###############################
#### Estimación de Modelos ####
###############################

#### Estimación del modelo 1 ####

carpeta = "Modelo 1/"
mod = " modelo 1"
modelo = peso ~ .
fit <- summary(lm(modelo, data=naci))

sink(file=paste0(carpeta, mod, ".txt"))
fit
"AIC"
AIC(lm(modelo, data=naci))
"BIC"
BIC(lm(modelo, data=naci))
sink()

#### Diagnóstico modelo 1 ####

resi=fit$residuals
sresi=scale(fit$residuals)
fitted=fitted(lm(modelo,data=naci))

sink(file=paste0(carpeta, "Normalidad", mod, ".txt"))
shapiro.test(resi)
jarque.bera.test(resi)
sink()

# qq plot

pdf(file=paste0(carpeta, "Histograma residuos", mod, ".pdf"))
ggplot(naci, aes(x=resi)) +
      geom_histogram(bins=20) +
      theme(axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            plot.title=element_text(size=16, face="bold", hjust=0.5),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Residuos")
dev.off()

p1 <- ggplot(naci, aes(x=fitted, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Fitted", y="Standarized residuals")

p2 <- ggplot(naci, aes(x=fuma, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_x_discrete(labels=c("si"="Si","no"="No")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Fuma", y="Standarized residuals")

p3 <- ggplot(naci, aes(x=gest, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Tiempo de gestación")

p4 <- ggplot(naci, aes(x=educ, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Años de educación")

pdf(file=paste0(carpeta, "Diagnóstico", mod, ".pdf"), width=9, height=6)
multiplot(p1,p2,p3,p4, cols=2)
dev.off()

pdf(file=paste0(carpeta,"Autocorrelación", mod, ".pdf"), width=9, height=6)
ggplot(naci, aes(x=seq(1:dim(naci)[1]), y=sresi)) +
      geom_line(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Observaciones")
dev.off()

pdf(file=paste0(carpeta, "Fitted vs peso", mod, ".pdf"), width=9, height=6)
ggplot(naci, aes(x=fitted, y=peso)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Fitted", y="Peso")
dev.off()

#### White's test modelo 1 ####

resi2=resi*resi
white <- summary(lm(resi2 ~ educ*gest + educ*fuma + fuma*gest + 
                          I(educ^2) + I(gest^2), data=naci))
lambda=white$r.squared*dim(naci)[1]
chi=qchisq(0.95, df=dim(white$coefficients)[1]-1)

sink(file=paste0(carpeta, "White test", mod, ".txt"))
white
print(c("lambda =", lambda))
print(c("chi = ", chi))
if(lambda > chi){
      print("Rechazo H0 => Hetero")
}else{
      print("No rechazo H0 => Homo")
}
sink()

#### CROSS VALIDATION modelo 1 ####

# LOOCV
glm.fit <- glm(modelo, data=naci)
cv.err.loocv <- cv.glm(data=naci, glmfit=glm.fit)
cv.err.loocv$delta[1]

# k fold CV
glm.fit <- glm(modelo, data=naci)
cv.err.kfold <- cv.glm(data=naci, glmfit=glm.fit, K=10)
cv.err.kfold$delta[1]

sink(file=paste0(carpeta, "Cross Validation", mod, ".txt"))
cv.err.loocv$delta[1]
cv.err.kfold$delta[1]
sink()

#### Estimación del modelo 2 ####

carpeta = "Modelo 2/"
mod = " modelo 2"
modelo = peso ~ educ + fuma + gest + I(gest^3)
fit <- summary(lm(modelo, data=naci))

sink(file=paste0(carpeta, mod, ".txt"))
fit
"AIC"
AIC(lm(modelo, data=naci))
"BIC"
BIC(lm(modelo, data=naci))
sink()

#### Diagnóstico modelo 2 ####

resi=fit$residuals
sresi=scale(fit$residuals)
fitted=fitted(lm(modelo,data=naci))

sink(file=paste0(carpeta, "Normalidad", mod, ".txt"))
shapiro.test(resi)
jarque.bera.test(resi)
sink()

# hacer qq plot

pdf(file=paste0(carpeta, "Histograma residuos", mod, ".pdf"))
ggplot(naci, aes(x=resi)) +
      geom_histogram(bins=20) +
      theme(axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            plot.title=element_text(size=16, face="bold", hjust=0.5),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Residuos")
dev.off()

p1 <- ggplot(naci, aes(x=fitted, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Fitted", y="Standarized residuals")

p2 <- ggplot(naci, aes(x=fuma, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_x_discrete(labels=c("si"="Si","no"="No")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Fuma", y="Standarized residuals")

p3 <- ggplot(naci, aes(x=gest, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Tiempo de gestación")

p4 <- ggplot(naci, aes(x=educ, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Años de educación")

pdf(file=paste0(carpeta, "Diagnóstico", mod, ".pdf"), width=9, height=6)
multiplot(p1,p2,p3,p4, cols=2)
dev.off()

pdf(file=paste0(carpeta,"Autocorrelación", mod, ".pdf"), width=9, height=6)
ggplot(naci, aes(x=seq(1:dim(naci)[1]), y=sresi)) +
      geom_line(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Observaciones")
dev.off()

pdf(file=paste0(carpeta, "Fitted vs peso", mod, ".pdf"), width=9, height=6)
ggplot(naci, aes(x=fitted, y=peso)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Fitted", y="Peso")
dev.off()

pdf(file=paste0(carpeta, "Cook's distance", mod, ".pdf"), width=9, height=6)
par(oma=c(1,1,1,1))
plot(lm(modelo, data=naci), which=4, pch=16, col="darkgreen")
dev.off()

pdf(file=paste0(carpeta, "resi vs lev", mod, ".pdf"), width=9, height=6)
par(oma=c(1,1,1,1))
plot(lm(modelo, data=naci), which=5, pch=16, col="darkgreen")
dev.off()

pdf(file=paste0(carpeta, "cook vs lv", mod, ".pdf"), width=9, height=6)
par(oma=c(1,1,1,1))
plot(lm(modelo, data=naci), which=6, pch=16, col="darkgreen")
dev.off()

regresores <- dplyr::select(naci, fuma, gest, educ)
agnes_sl <- agnes(regresores, metric="euclidean", stand=TRUE, method="single")

pdf(file=paste0(carpeta, "agnes", mod, ".pdf"), width=9, height=6)
par(oma=c(1,1,1,1))
plot(agnes_sl, which=2, nmax.lab=50, main="Single Linkage (dist: euclidean)", xlab=NA, cex.main=2)
dev.off()


#### White's test modelo 2 ####

resi2=resi*resi
white <- summary(lm(resi2 ~ educ*gest + educ*fuma + educ*I(gest^3) +
                          gest*fuma + gest*I(gest^3) + fuma*I(gest^3) +
                          I(educ^2) + I(gest^2) + I(I(gest^3)^2), 
                    data=naci))
lambda=white$r.squared*dim(naci)[1]
chi=qchisq(0.95, df=dim(white$coefficients)[1]-1)

sink(file=paste0(carpeta, "White test", mod, ".txt"))
white
print(c("lambda =", lambda))
print(c("chi = ", chi))
if(lambda > chi){
      print("Rechazo H0 => Hetero")
}else{
      print("No rechazo H0 => Homo")
}
sink()

#### CROSS VALIDATION modelo 2 ####

# LOOCV
glm.fit <- glm(modelo, data=naci)
cv.err.loocv <- cv.glm(data=naci, glmfit=glm.fit)
cv.err.loocv$delta[1]

# k fold CV
glm.fit <- glm(modelo, data=naci)
cv.err.kfold <- cv.glm(data=naci, glmfit=glm.fit, K=10)
cv.err.kfold$delta[1]

sink(file=paste0(carpeta, "Cross Validation", mod, ".txt"))
cv.err.loocv$delta[1]
cv.err.kfold$delta[1]
sink()

#### BEST SUBSET SELECTION modelo 2 ####

regfit.full <- regsubsets(modelo, naci)
reg.sum <- summary(regfit.full)

X11(15,15)
par(mfrow=c(2,2), oma=c(1,1,1,1))
plot(reg.sum$rss, xlab=NA, ylab=" RSS", type="l")
plot(reg.sum$adjr2, xlab=NA, ylab=" Adjusted RSq", type="l")
points(which.max (reg.sum$adjr2), reg.sum$adjr2[which.max (reg.sum$adjr2)], col="red", cex=2, pch=20)
plot(reg.sum$cp, xlab="Number of Variables", ylab="Cp", type="l")
points(which.min(reg.sum$cp), reg.sum$cp[which.min(reg.sum$cp)], col="red", cex=2, pch=20)
plot(reg.sum$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(which.min(reg.sum$bic), reg.sum$bic[which.min(reg.sum$bic)], col="red", cex=2, pch=20)

X11(15,15)
par(mfrow=c(2,2))
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full, which.max(reg.sum$adjr2))
coef(regfit.full, which.min(reg.sum$cp))
coef(regfit.full, which.min(reg.sum$bic))

#### STEPWISE SELECTION modelo 2 ####

regfit.fwd <- regsubsets(modelo, data=naci, method="forward")
summary(regfit.fwd)

coef(regfit.fwd, which.max(summary(regfit.fwd)$adjr2))
coef(regfit.fwd, which.min(summary(regfit.fwd)$cp))
coef(regfit.fwd, which.min(summary(regfit.fwd)$bic))

regfit.bwd <- regsubsets(modelo, data=naci, method="backward")
summary(regfit.bwd)

coef(regfit.fwd, which.max(summary(regfit.bwd)$adjr2))
coef(regfit.fwd, which.min(summary(regfit.bwd)$cp))
coef(regfit.fwd, which.min(summary(regfit.bwd)$bic))

#### STEPWISE CROSS-VALIDATION AND VALIDATION SET modelo 2 ####

set.seed(100)
train <- sample(c(T,F), nrow(naci), rep=T)
test <- (!train)

regfit <- regsubsets(modelo, naci[train,])
test.mat <- model.matrix(modelo, data=naci[test,])
val.errors <- rep(NA, length(attr(attr(model.frame(modelo, naci), "terms"),"term.labels")))
for(i in 1:length(attr(attr(model.frame(modelo, naci), "terms"),"term.labels"))){
      coefi <- coef(regfit, id=i)
      pred <- test.mat[,names(coefi)] %*% coefi
      val.errors[i] <- mean((naci$peso[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(object=regsubsets(modelo, data=naci), id=which.min(val.errors))


k=10
set.seed(100)
folds <- sample(1:k, nrow(naci), replace=TRUE)
cv.errors <- matrix(NA, k, length(attr(attr(model.frame(modelo, naci), "terms"),"term.labels")), dimnames=list(NULL, paste(1:length(attr(attr(model.frame(modelo, naci), "terms"),"term.labels")))))
for(j in 1:k){
      best.fit <- regsubsets(modelo, data=naci[folds!=j,], nvmax=length(attr(attr(model.frame(modelo, naci), "terms"),"term.labels")))
      for(i in 1:length(attr(attr(model.frame(modelo, naci), "terms"),"term.labels"))){
            coefi <- coef(best.fit, id=i)
            pred <- test.mat[,names(coefi)] %*% coefi
            cv.errors[j,i] <- mean((naci$peso[test]-pred)^2)
      }
}
cv.errors

mean.cv.errors <- apply(cv.errors, 2, mean)

X11(15,15)
par(oma=c(1,1,1,1))
plot(mean.cv.errors, type="b", col="red", pch=20)

coef(regsubsets(modelo, data=naci), which.min(mean.cv.errors))


#### RIDGE AND LASSO modelo 2 ####

y <- naci$peso
x <- model.matrix(modelo, data=naci)[,-1]

# Ridge
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

# write.csv(t(as.matrix(coef(ridge.mod))), file=paste0(carpeta, "Ridge coefs", mod, ".csv"))

predict(ridge.mod, s=50, type="coefficients")

# Cross-validation
set.seed(100)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

# Cross-validating lambda

set.seed(100)
cv.out <- glmnet::cv.glmnet(x[train,], y[train], alpha=0)

X11(15,15)
plot(cv.out)

bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

out <- glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)

# Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)

plot(lasso.mod)

set.seed(100)
cv.out <- glmnet::cv.glmnet(x[train,], y[train], alpha=1)

plot(cv.out)

bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)
lasso.coef

#### Dfbetas, dffitts, cooks modelo 2 ####

influ <- influence.measures(lm(modelo, naci))
write.csv(influ[[1]], file="Modelo 2/influ2.csv")

#### Estimaci?n del modelo 3 ####

carpeta = "Modelo 3/"
mod = " modelo 3"
modelo = peso ~ educ + gest + gest:fuma
fit <- summary(lm(modelo, data=naci))

sink(file=paste0(carpeta, mod, ".txt"))
fit
AIC(lm(modelo, data=naci))
BIC(lm(modelo, data=naci))
sink()

#### Diagnóstico modelo 3 ####

resi=fit$residuals
sresi=scale(fit$residuals)
fitted=fitted(lm(modelo,data=naci))

sink(file=paste0(carpeta, "Normalidad", mod, ".txt"))
shapiro.test(resi)
jarque.bera.test(resi)
sink()

# qq plot

pdf(file=paste0(carpeta, "Histograma residuos", mod, ".pdf"))
ggplot(naci, aes(x=resi)) +
      geom_histogram(bins=20) +
      theme(axis.title.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            plot.title=element_text(size=16, face="bold", hjust=0.5),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Residuos")
dev.off()

p1 <- ggplot(naci, aes(x=fitted, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Fitted", y="Standarized residuals")

p2 <- ggplot(naci, aes(x=fuma, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_x_discrete(labels=c("si"="Si","no"="No")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Fuma", y="Standarized residuals")

p3 <- ggplot(naci, aes(x=gest, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Tiempo de gestación")

p4 <- ggplot(naci, aes(x=educ, y=sresi)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Años de educación")

pdf(file=paste0(carpeta, "Diagnóstico", mod, ".pdf"), width=9, height=6)
multiplot(p1,p2,p3,p4, cols=2)
dev.off()

pdf(file=paste0(carpeta,"Autocorrelación", mod, ".pdf"), width=9, height=6)
ggplot(naci, aes(x=seq(1:dim(naci)[1]), y=sresi)) +
      geom_line(colour="dodgerblue", alpha=0.5) +
      geom_abline(intercept=mean(sresi), slope=0, color="red") +
      geom_abline(intercept=2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=3, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-2, slope=0, color="red", linetype="dashed") +
      geom_abline(intercept=-3, slope=0, color="red", linetype="dashed") +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_blank(),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      scale_y_continuous(breaks=c(-3,-2,0,2,3), labels=c(-3,-2,0,2,3)) +
      labs(x="Observaciones")
dev.off()

pdf(file=paste0(carpeta, "Fitted vs peso", mod, ".pdf"), width=9, height=6)
ggplot(naci, aes(x=fitted, y=peso)) +
      geom_point(colour="dodgerblue", alpha=0.5) +
      theme(text=element_text(family="sans"),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title.x=element_text(face="bold", size=12),
            axis.title.y=element_text(face="bold", size=12),
            plot.margin=unit(c(.5,.5,.5,.5), "cm")) +
      labs(x="Fitted", y="Peso")
dev.off()


#### White's test modelo 3 ####

resi2=resi*resi
white <- summary(lm(resi2 ~ educ + gest + educ:gest + I(educ^2) + 
                          I(gest^2) + gest:fuma + I(gest^2):fuma,
                    data=naci))
lambda=white$r.squared*dim(naci)[1]
chi=qchisq(0.95, df=dim(white$coefficients)[1]-1)

sink(file=paste0(carpeta, "White test", mod, ".txt"))
white
print(c("lambda =", lambda))
print(c("chi = ", chi))
if(lambda > chi){
      print("Rechazo H0 => Hetero")
}else{
      print("No rechazo H0 => Homo")
}
sink()

#### CROSS VALIDATION modelo 3 ####

# LOOCV
glm.fit <- glm(modelo, data=naci)
cv.err.loocv <- cv.glm(data=naci, glmfit=glm.fit)
cv.err.loocv$delta[1]

# k fold CV
glm.fit <- glm(modelo, data=naci)
cv.err.kfold <- cv.glm(data=naci, glmfit=glm.fit, K=10)
cv.err.kfold$delta[1]

sink(file=paste0(carpeta, "Cross Validation", mod, ".txt"))
cv.err.loocv$delta[1]
cv.err.kfold$delta[1]
sink()

#### ANOVA ####

anova(lm(peso ~ fuma + educ, data=naci))

#### OTROS MODELOS ####

# Polinomios de educación no significativos
summary(lm(peso~gest+fuma+poly(educ,2), data=naci))
summary(lm(peso~gest+fuma+poly(educ,3), data=naci))
summary(lm(peso~gest+fuma+educ+I(educ^3), data=naci))

# Polinomios de gestación no significativos
summary(lm(peso~educ+fuma+poly(gest,2), data=naci))
summary(lm(peso~educ+fuma+poly(gest,3), data=naci))

# Polinomios de educación gestación no significativos
summary(lm(peso ~ poly(educ, 2) + fuma + poly(gest, 2), data=naci))

# Interacción de fuma y gestación
summary(lm(peso ~ educ + fuma + gest:fuma, data=naci))
summary(lm(peso ~ educ + gest + fuma + gest:fuma, data=naci))
summary(lm(peso ~ educ + gest:fuma, data=naci))

###############################
#### FIN DE LA PROGRAMCIÓN ####
###############################