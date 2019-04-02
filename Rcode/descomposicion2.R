rm(list=ls())
paquetes <- c("tidyverse","lubridate","dlm","ggfortify","smoother","KernSmooth","xtable","latex2exp")
sapply(paquetes,require,character.only=T)
load(file="datos_resultados/DLMmod.RData")

minimas <- read.table("Tn_1950_2014_con_QC_v1.txt", sep = ',', 
                      na.strings = "NaN") %>%
  set_names(nm = c("year", "month", "day","artigas", "carrasco", "estancuela", 
                   "melo", "mercedes", "paso_toros", 
                   "paysandu", "prado", "rivera", "rocha", "salto")  ) %>%
  mutate(fecha = make_date(year, month, day)) %>%
  as_data_frame()

maximas <- read.table("Tx_1950_2014_con_QC_v1.txt", sep = ',', 
                      na.strings = "NaN") %>%
  set_names(nm = c("year", "month", "day","artigas", "carrasco", "estancuela", 
                   "melo", "mercedes", "paso_toros", 
                   "paysandu", "prado", "rivera", "rocha", "salto")  ) %>%
  mutate(fecha = make_date(year, month, day)) %>%
  as_data_frame()

Tn.bar <-mean(minimas$estancuela,na.rm=T)
Tx.bar <-mean(maximas$estancuela,na.rm=T)

Tn <- minimas %>% mutate(
  month=formatC(minimas$month,width=2,format="d",flag="0"),
  day=formatC(minimas$day,width=2,format="d",flag="0"),
  dia=paste(month,day,sep="-"),
  temp=estancuela-Tn.bar) %>% select(c(dia,year,fecha,temp))

Tx <- maximas %>% mutate(
  month=formatC(maximas$month,width=2,format="d",flag="0"),
  day=formatC(maximas$day,width=2,format="d",flag="0"),
  dia=paste(month,day,sep="-"),
  temp=estancuela-Tx.bar) %>% select(c(temp))

serie <- cbind(Tn,Tx)
names(serie)[4:5] <- c("Tn","Tx")

# lm
n <- length(Tn$temp)
t <- 1:n
lm(Tn$temp ~I(cos(t*2*pi/365))) %>% summary
lm(Tx$temp ~I(cos(t*2*pi/365))) %>% summary

# estimacion MLE ### demora
parMLE <- function(par)  dlmModTrig(s = 365,q=2,dV =par[1], dW = 0)
#Tn.mod<- dlmMLE(Tn$temp,parm = 16,build= parMLE)
#Tx.mod<- dlmMLE(Tn$temp,parm = 20,build= parMLE)


# filtro de Kalman
Tn.filter <- dlmFilter(Tn$temp,parMLE(Tn.mod$par))
Tx.filter <- dlmFilter(Tx$temp,parMLE(Tx.mod$par))

serie$Tn.pred <-  (Tn.filter$m[-1,1]+Tn.filter$m[-1,3])
serie$Tx.pred <-  (Tx.filter$m[-1,1]+Tx.filter$m[-1,3])

# Matrices de varianzas
Mn <- sapply(1:n,function(l) dlmSvd2var(Tn.filter$U.R[[l]], Tn.filter$D.R[l,])[c(1,3),c(1,3)]) %>% array(dim = c(2,2,n))
Mx <- sapply(1:n,function(l) dlmSvd2var(Tx.filter$U.R[[l]], Tx.filter$D.R[l,])[c(1,3),c(1,3)]) %>% array(dim = c(2,2,n))

var.n <- sapply(1:n,function(i) sum(diag(Mn[,,i]))+2*Mn[1,2,i])+Tn.mod$par
var.x <- sapply(1:n,function(i) sum(diag(Mx[,,i]))+2*Mx[1,2,i])+Tx.mod$par

#

serie <- serie %>% mutate_at(vars(c(Tn.pred,Tn)),function (x) x+Tn.bar)
serie <- serie %>% mutate_at(vars(c(Tx.pred,Tx)),function (x) x+Tx.bar)
serie <- serie %>% mutate(p10n=Tn.pred+sqrt(var.n)*qnorm(.1),p10x=Tx.pred+sqrt(var.x)*qnorm(.1))
serie %>% head


##################################################
##################################################
##################################################
# olas


## ----olas,fig.cap='Serie de mínimas con olas de frío'--------------------
frio <- (serie$Tn<serie$p10n & serie$Tx<serie$p10x) %>% as.numeric
#(frio+lag(frio)+lag(frio,2)==3) %>% as.numeric %>% sum(na.rm=T)
ola <- (frio+lag(frio)+lag(frio,2)==3) %>% as.numeric
ola <- frio+lag(frio)+lag(frio,2)
ola[is.na(ola)] <- 0

es_ola <- rep(0,length(ola))
sapply(3:length(ola),function(i) if (ola[i]==3) {es_ola[i-2] <<- 3; es_ola[i-1] <<- 3 ; es_ola[i] <<-3}) %>% invisible 
es_ola <- es_ola/3

#serie$ola <- c(ola[-(1:2)],0,0) #comienzo de la ola
#serie[serie$ola==1,] %>% View()
serie$ola <- factor(es_ola)

######################## fin de olas

# graficos
#dat <- serie[(365*10):(365*14),] %>% select(fecha,Tn,Tx,p10n,p10x,ola) %>% gather(serie,temp,-fecha,-ola) %>% mutate(grup=ifelse((serie=='Tn' | serie=='p10n'),1,0))
dat <- serie %>% filter(between(year,1962,1962)) %>% select(fecha,Tn,Tx,p10n,p10x,ola) %>% gather(serie,temp,-fecha,-ola) %>% mutate(grup=ifelse((serie=='Tn' | serie=='p10n'),"Mínima","Máxima"))
dat <- serie %>% filter(between(year,1966,1968)) %>% select(fecha,Tn,Tx,p10n,p10x,ola) %>% gather(serie,temp,-fecha,-ola) %>% mutate(grup=ifelse((serie=='Tn' | serie=='p10n'),"Mínima","Máxima"))

serie %>% head 

# grafico 1, serie diaria de minimas y maximas
serie %>% select(dia:Tx) %>%  gather(serie,temp,-dia,-fecha,-year) %>%
  ggplot() + aes(x=fecha,y=temp,col=serie) + geom_line()

# grafico 2, minimas y maximas por dia
serie %>% select(dia:Tx) %>% gather(serie,temp,-dia,-fecha,-year) %>%
  ggplot() + aes(x=dia,y=temp,col=serie) + geom_line()+facet_wrap(serie~.)

serie %>% select(dia:Tx) %>% gather(serie,temp,-dia,-fecha,-year) %>%
  ggplot() + aes(x=dia,y=temp,col=serie) + geom_line()+facet_grid(serie~.)

# grafico 3
serie[(365*60):(365*64),] %>% select(-(Tn.pred:ola)) %>% 
  gather(serie,temp,-dia,-year,-fecha) %>% ggplot() + aes(x=fecha,y=temp,col=serie)+geom_line()

# grafico 4
colores <- c('#F8766D','#7CAE00','#00BFC4','#C77CFF')et <- c("p10.Mín","p10.Máx","Mín","Máx")
myguide <- guide_legend(keywidth = unit(2, "cm"))
p <- ggplot(dat,aes(x=fecha,y=temp,col=serie))+geom_line(lwd=1)+geom_point(size=.1) + facet_wrap(~grup,nrow=2)
p+geom_point(aes(x=fecha,y=temp,size='ola de frío'),data = filter(dat,ola==1,(serie=="Tn" | serie=="Tx")),col="chocolate1")+
  ylab("Temperatura")+xlab("Fecha")+scale_size_manual(values=c("ola de frío"=2),guide=myguide,name=NULL)+scale_color_manual(labels=et,values=colores,guide=myguide)+theme_bw()
ggsave("graf4.png",scale = 3,path = "poster")

# grafico 5, rachas
myguide <- guide_legend(keywidth = unit(2, "cm"))
ggplot(serie)+aes(x=dia,y=year,col=ola)+geom_point()+scale_color_manual(values=c('#F8766D',"blue"),labels=c("","ola de frío"),name=NULL,guide=myguide)+
  theme_bw()
ggsave("graf5.png",scale = 3,path = "poster")

# grafico 6, temperaturas y filtro de kalman
myguide <- guide_legend(keywidth = unit(2, "cm"))
p6 <- serie %>% filter(between(year,1966,1968)) %>% select(-(p10n:ola)) %>% gather(serie,temp,-dia,-year,-fecha) %>% 
  ggplot()+aes(x=fecha,y=temp,col=serie)+geom_line()+ylab("Temperatura")+xlab("Fecha")+theme_bw()
etiquetas <- c('Mínima','Mín. esperada','Máxima','Máx. esperada')
p6 + scale_color_manual(values=colores,labels=etiquetas,guide=myguide)+theme()
ggsave("graf6.png",scale = 3,path = "poster")
# g <- ggplot_build(p6)
# unique(g$data[[1]]["colour"]) %>% print(include.names=F)


# grafico 7, dlm general
d <- faithful$eruptions %>% density
d <- (d$y-mean(d$y))^2
m <- length(d)
set.seed(1234)
w <- rnorm(m,0,.01)
theta <- (10*d+w)
v <- rnorm(m,0,.09)
x <- theta + v
p7 <- data.frame(ind=1:m,x,theta)  %>%  gather(serie,valor,-ind) %>% 
  cbind(tipo=factor(c(rep(.5,m),rep(1,m)))) %>%
  ggplot()+aes(x=ind,y=valor,col=serie,alpha=tipo)+geom_line()
x11()
p7+scale_alpha_discrete(range = c(.5,1),guide=F)+scale_color_manual(values=c("theta"='#F8766F',"x"='#7CAE00'),guide=myguide)+theme_bw()

#RGD <- dev.cur()
#extra <- dev.cur()
extra
RGD
dev.set(which=RGD)
dev.set(which=extra)
dev.off()

f <- function(x) #(x-1)*(x-2)*(x-3)*(x-4)*(x-5)
-120* x + 137* x^2 - 75* x^3 + (85* x^4)/4 - 3* x^5 + x^6/6 -x^7/100000 +40
curve(f,0.5,5.5)

ggsave("graf7.png",scale = 3,path = "poster")

# invento dlm -------------------------------------------------------------

set.seed(1237)
N <- 100
t <- seq(0,1,,N)
#t <- 1:N
g <- rep(1,N)
w <- rnorm(N,0,.1)
v <- rnorm(N,0,.3)
theta <- cumsum(w)
plot(t,theta,type="l")
y <- theta+v
lines(t,y,col=2)
myguide <- guide_legend(keywidth = unit(0, "cm"))
p7.1 <- data.frame(t,y,theta)  %>%  gather(serie,valor,-t) %>% 
  cbind(tipo=factor(c(rep(.5,N),rep(1,N)))) %>%
  ggplot()+aes(x=t,y=valor,col=serie,alpha=tipo)+geom_line()+
  scale_alpha_discrete(range = c(.5,1),guide=F)+scale_color_manual(values=c("theta"='#F8766F',"y"='#7CAE00'),guide=myguide)+theme_bw()

p7.1 <- data.frame(t,y,theta)  %>%  gather(serie,valor,-t) %>% 
  cbind(tipo=factor(c(rep(.5,N),rep(1,N)))) %>%
  ggplot()+aes(x=t,y=valor,col=serie,alpha=tipo)+geom_line()+
  scale_alpha_discrete(range = c(.5,1),guide=F)+scale_color_manual(values=c("theta"='#F8766F',"y"='#7CAE00'))+theme_bw()+theme(legend.position = "none")

pdf("graf7.1.pdf",height = 4,width = 6)
print(p7.1)
dev.off()

# a -----------------------------------------------------------------------



# tabla 1, racha
tabla1 <- serie[6369:6376,c(3,4,8,5,9)] 
names(tabla1) <- c("Fecha","Mín","Máx","p10 mín","p10 máx")
tabla1[,1] <- tabla1[,1] %>% as.character
tabla1 %>% xtable %>% print(include.rownames=F)

#save.image(file="DLMmod.RData")

