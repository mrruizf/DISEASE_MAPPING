library(spdep)       ##Matriz de contiguidad
library(INLA)        ##APROXIMACIÓN DE LAPLACE Y MODELADO
library(dplyr)
library(maptools)
library(tmap)
library(ggplot2)
library(tidyr)
library(SpatialEpi)
library(xtable)
library(rgdal)
MPIOS<-readOGR("G:/Shapefiles/Epidemiologia/EPIDEMIOLOGIA.shp")
datos<-as.data.frame(MPIOS@data)

datos<-tbl_df(datos)
datos%>%select(cod_dane,nom_munici,nom_depart,pob_hombre,sex_mascul)
POB.HOMB<-select(datos,cod_dane,nom_munici,nom_depart,pob_hombre,sex_mascul)
POB.HOMB$SEX<-"HOMBRE"
colnames(POB.HOMB)[4] <- "POB"
colnames(POB.HOMB)[5] <- "CASOS"

POB.MUJ<-select(datos,cod_dane,nom_munici,nom_depart,pob_mujer,sex_femeni)
POB.MUJ$SEX<-"MUJER"
colnames(POB.MUJ)[4] <- "POB"
colnames(POB.MUJ)[5] <- "CASOS"

POBLACION<-rbind(POB.HOMB,POB.MUJ)
POBLACION<-POBLACION[order(POBLACION$cod_dane), ]
datos<-datos[order(datos$cod_dane), ]

E <- expected(POBLACION$POB, POBLACION$CASOS, 2)

datos$E <-E

############################################################################
############################################################################
#############MATRIZ DE PESOS ESPACIALES GENERADA EN EL CÓDIGO 3#############
############################################################################
############################################################################

coordenadas<-coordinates(MPIOS)    #k-vecinos
k5 <- knn2nb(knearneigh(coordenadas,5))

###Mtriz de contiguidad seleccionada
nb2INLA("MPIO.graph", k5)
MPIOS.adj <- paste(getwd(),"/MPIO.graph",sep="")
H <- inla.read.graph(filename="MPIO.graph")
###opcion 1
image(inla.graph2matrix(H),xlab="",ylab="")
###opcion 2
coordenadas<-coordinates(MPIOS)
x11()
plot(MPIOS,border=gray(0.5))%&%
  plot(TEMP,coords=coordenadas,add=T,pch=16,lwd=2)

#########################################################################
#########################################################################
#########################################################################
###################MODELO BYM CON DISTRIBUCIÓN POISSON###################
#########################################################################
#########################################################################
#########################################################################
datos['index'] <- 1:1120
##
##estandarización de variables continuas
Mystd<-function(x) {(x-mean(x))/sd(x)}
datos$bosque_std<-Mystd(datos$bosque_bos)
datos$preci_std<-Mystd(datos$p_preci_me)
datos$alt_med_std<-Mystd(datos$alt_med)
##POISSON INFORMATIVO
formula.poisson<- mala_total~1+valor+valor_1+deng_total+chic_total+zika_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj,
    hyper=list(prec.unstruct=list(prior="gamma",param=c(107.121,0.9824992)),
               prec.spatial=list(prior="gaussian",param=c(0,1))))

modelo.poisson <- inla(formula.poisson,family="poisson",
                           data=datos,E=E,verbose = T,control.inla=list(cmin = 0),
                           control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE))

summary(modelo.poisson)
round(modelo.poisson$summary.fixed,6)$mean*100
EXPBO<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[1]])
EXPVAL<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[2]])
EXPVAL1<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[3]])
expdenge<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[4]])
expchic<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[5]])
expzika<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[6]])
expbosq<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[7]])
exppreci<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[8]])
expalt<-inla.emarginal(exp,modelo.poisson$marginals.fixed[[9]])

round((c(EXPBO,EXPVAL,EXPVAL1,expdenge,expchic,expzika,expbosq,exppreci,expalt)-1)*100,6)
print(c(EXPBO,EXPVAL,EXPVAL1,expdenge,expchic,expzika,expbosq,exppreci,expalt))
##densidad betas
library(reshape2)
mf<-melt(modelo.poisson$marginals.fixed)
cf <- spread(mf,Var2,value)
names(cf)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                    scales="free") + geom_vline(xintercept=0) + ylab("density")

############################################################################
############################################################################
##########################MAPA DE RIESGO RELATIVO###########################
############################################################################
############################################################################

MPIOS$RRPoisson<- modelo.poisson$summary.fitted.values[, "mean"]
X11()
brks.p<-c(0.1,0.8,0.9,0.95, 1.0,1.05,1.1,1.2,1.3,1.4, 1.5,2,15,80)
tm_shape(MPIOS) + 
  tm_polygons("RRPoisson",breaks=brks.p,title="Riesgo Relativo \n Poisson" ,palette = "YlOrRd",
              contrast = 0.7, border.col = "gray30",id = "name")+
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_legend(position = c("RIGTH", "TOP"))+
  tm_format_World() + 
  tm_style_gray(title = "Riesgo relativo de Malaria \npara cada municipio \nen Colombia \naño 2015",
                title.size=1)
############################################################################
############################################################################
###########################POISSON NO INFORMATIVO###########################
############################################################################
############################################################################

formula.poisson.NI<- mala_total~1+valor+valor_1+deng_total+chic_total+zika_total+bosque_std + preci_std + alt_med_std +
                  f(index, model="bym", graph=MPIOS.adj,
                    hyper=list(prec.unstruct=list(prior="gamma",param=c(0.5,0.001)),
                               prec.spatial=list(prior="gaussian",param=c(0,1))))

modelo.poisson.NI <- inla(formula.poisson.NI,family="poisson",
                       data=datos,E=E,verbose = T,control.inla=list(cmin = 0),
                       control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE))

round(modelo.poisson.NI$summary.fixed,6)
##densidad betas
library(reshape2)
mf.poiss.ni<-melt(modelo.poisson.NI$marginals.fixed)
cf.poiss.ni <- spread(mf.poiss.ni,Var2,value)
names(cf.poiss.ni)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.poiss.ni,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                 scales="free") + geom_vline(xintercept=0) + ylab("density")

v1<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$`(Intercept)`)
v2<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$valor)
v3<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$valor_1)
v4<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$deng_total)
v5<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$chic_total)
v6<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$zika_total)
v7<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$bosque_std)
v8<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$preci_std)
v9<-inla.emarginal(exp, modelo.poisson.NI$marginals.fixed$alt_med_std)
round((c(v1,v2,v3,v4,v5,v6,v7,v8,v9)-1)*100,6)
#########################################################################
#########################################################################
#########################################################################
###################MODELO BYM CON DISTRIBUCIÓN BINOMIAL##################
#########################################################################
#########################################################################
#########################################################################
######BINOMIAL NO INFORMATIVO
prior.function = function(simulated) {
  alpha=0.5;
  beta=0.5;
  logdens=pbeta(simulated,alpha,beta)
  return(logdens)
}

set.seed(13579)                               
N <- 10000
phi <- rbeta(N, shape1 = 0.5, shape2 = 0.5)
beta.prior = paste(c("table:", cbind(phi, prior.function(phi))),
                   sep = "", collapse = " ")


formula.binomial.NI<- mala_total~1+valor+valor_1+deng_total+chic_total+zika_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj,
    hyper=list(prec.unstruct=list(prior=beta.prior),
               prec.spatial=list(prior="gaussian",param=c(0,1))))

modelo.binomial.NI <- inla(formula.binomial.NI,family="binomial",
                           data=datos,E=E,verbose = T,Ntrials =total, 
                           control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE))

summary(modelo.binomial.NI)
##significacncia betas
round(modelo.binomial.NI$summary.fixed,6)

##densidad betas
library(reshape2)
mf.B<-melt(modelo.binomial.NI$marginals.fixed)
cf.B <- spread(mf.B,Var2,value)
names(cf.B)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.B,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                 scales="free") + geom_vline(xintercept=0) + ylab("density")


##interpretacion betas
prob.malaria <- inla.tmarginal(function(x) exp(x)/(1+exp(x)),
                               modelo.binomial.NI$marginals.fixed[[1]])
inla.zmarginal(prob.malaria)

bin1<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$valor)
bin2<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$valor_1)
bin3<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$deng_total)
bin4<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$chic_total)
bin5<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$zika_total)
bin6<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$bosque_std)
bin7<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$preci_std)
bin8<-inla.emarginal(exp, modelo.binomial.NI$marginals.fixed$alt_med_std)
round((c(bin1,bin2,bin3,bin4,bin5,bin6,bin7,bin8)-1)*100,6)
print(c(bin1,bin2,bin3,bin4,bin5,bin6,bin7,bin8))

Binomial.NI<-round(modelo.binomial.NI$summary.fixed,6)$mean*100

############################################################################
############################################################################
########################MAPA DE TASA DE  PREVALENCIA########################
############################################################################
############################################################################

MPIOS@data$RRBinomial<- modelo.binomial.NI$summary.fitted.values[, "mean"]
X11()
brks.p<-c(0,0.001,0.005,0.01,0.02,0.05,0.1,0.15,0.2,0.25)
tm_shape(MPIOS) + 
  tm_polygons("RRBinomial",breaks=brks.p,title="Riesgo Relativo \nBinomial" ,palette = "YlOrRd",
              contrast = 0.7, border.col = "gray30",id = "name")+
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_legend(position = c("RIGTH", "TOP"))+
  tm_format_World() + 
  tm_style_gray(title = "Riesgo relativo de Malaria \npara cada municipio \nen Colombia \naño 2015",
                title.size=1)

#########################################################################
###################MODELO BYM CON DISTRIBUCIÓN ##########################
##########################BINOMIAL NEGATIVA##############################
#########################################################################
#########################################################################

#########################################################################
######BINOMIAL NEGATIVO NO INFORMATIVO
formula.binomialNEG.NI<- mala_total~1+valor+valor_1+deng_total+chic_total+zika_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj)

modelo.binomialNEG.NI <- inla(formula.binomialNEG.NI,family="nbinomial2",
                              data=datos,verbose = T,E =E,control.family = list(link="logit"),
                              control.predictor = list(compute=T,link=1),
                              control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE))

round(modelo.binomialNEG.NI$summary.fixed,6)

formula.binomialNEG.NI2<- mala_total~1+valor+valor_1+deng_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj)

modelo.binomialNEG.NI2 <- inla(formula.binomialNEG.NI2,family="nbinomial2",
                               data=datos,verbose = T,E =E,control.family = list(link="logit"),
                               control.predictor = list(compute=T,link=1),
                               control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE))

round(modelo.binomialNEG.NI2$summary.fixed,6)
summary(modelo.binomialNEG.NI2)
##densidad betas
library(reshape2)
mf.bn.noinf<-melt(modelo.binomialNEG.NI2$marginals.fixed)
cf.bn.noinf <- spread(mf.bn.noinf,Var2,value)
names(cf.bn.noinf)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.bn.noinf,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                          scales="free") + geom_vline(xintercept=0) + ylab("density")

#####aporte de los betas
EXPBO.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[1]])
EXPVAL.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[2]])
EXPVAL1.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[3]])
expdenge.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[4]])
expbosq.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[5]])
exppreci.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[6]])
expalt.nb.noinf<-inla.emarginal(exp,modelo.binomialNEG.NI2$marginals.fixed[[7]])


round((c(EXPBO.nb.noinf,EXPVAL.nb.noinf,EXPVAL1.nb.noinf,expdenge.nb.noinf,expbosq.nb.noinf,exppreci.nb.noinf,expalt.nb.noinf)-1)*100,6)
print(c(EXPBO.nb.noinf,EXPVAL.nb.noinf,EXPVAL1.nb.noinf,expdenge.nb.noinf,expbosq.nb.noinf,exppreci.nb.noinf,expalt.nb.noinf))

#####################################################################
#####################################################################
############################NB INFORMATIVO###########################
#####################################################################
#####################################################################
formula.binomialNEG<- mala_total ~ 1 + valor + valor_1 + deng_total + chic_total + 
  zika_total + bosque_std + preci_std + alt_med_std + 
  f(index,model = "bym", graph = MPIOS.adj, 
    hyper = list(prec.unstruct = list(prior = "pc.mgamma",param = c(26.2))))

modelo.binomialNEG<- inla(formula = formula.binomialNEG, family = nbinomial2, data = datos,
     E = E, verbose = T, control.compute = list(dic = TRUE, waic =TRUE,cpo = TRUE), control.predictor = list(compute = T, link = 1), 
     control.family = list(link = logit))

round(modelo.binomialNEG$summary.fixed,6)

##densidad betas
library(reshape2)
mf.bn<-melt(modelo.binomialNEG$marginals.fixed)
cf.bn <- spread(mf.bn,Var2,value)
names(cf.bn)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.bn,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                    scales="free") + geom_vline(xintercept=0) + ylab("density")

#####aporte de los betas
EXPBO.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[1]])
EXPVAL.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[2]])
EXPVAL1.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[3]])
expdenge.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[4]])
expchicu.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[5]])
expzika.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[6]])
expbosq.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[7]])
exppreci.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[8]])
expalt.nb<-inla.emarginal(exp,modelo.binomialNEG$marginals.fixed[[9]])

round((c(EXPBO.nb,EXPVAL.nb,EXPVAL1.nb,expdenge.nb,expchicu.nb,expzika.nb,expbosq.nb,exppreci.nb,expalt.nb)-1)*100,6)
print(c(EXPBO.nb,EXPVAL.nb,EXPVAL1.nb,expdenge.nb,expchicu.nb,expzika.nb,expbosq.nb,exppreci.nb,expalt.nb))

############################################################################
############################################################################
##########################MAPA DE RIESGO RELATIVO###########################
############################################################################
############################################################################

MPIOS$RRBN<- modelo.binomialNEG$summary.fitted.values[, "mean"]
writeOGR(MPIOS,"G:/","MPIOS", driver="ESRI Shapefile")
X11()
brks.p<-c(0.29,0.32,0.35,0.38,0.41,0.44,0.47,0.5,0.53,0.56,0.6)
tm_shape(MPIOS) + 
  tm_polygons("RRBN",breaks=brks.p,title="Riesgo Relativo \nNB" ,palette = "YlOrRd",title.size=0.5,
              contrast = 0.7, border.col = "gray30",id = "name")+
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_legend(position = c("RIGTH", "TOP"),legend.format = list(text.to.columns=T))+
  tm_format_World() + 
  tm_style_gray(title = "Riesgo relativo de Malaria \npara cada municipio \nen Colombia \naño 2015",
                title.size=1)

########################################################################
###################MODELO BYM CON DISTRIBUCIÓN POISSON##################
##########################CEROS INFLADOS (ZIP)##########################
##############################(1) MUESTRALES############################
########################################################################

#########ZIP NO INFORMATIVO
formula.zip.NI<- mala_total~1+valor+valor_1+deng_total+chic_total+zika_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj,
    hyper=list(prec.unstruct=list(prior="logitbeta",param=c(0.5,0.5), initial=0),
               prec.spatial=list(prior="gaussian",param=c(0,1))))

mod.malaria.zip.NI<- inla(formula.zip.NI,family="zeroinflatedpoisson1",
                          data=datos, offset = log(E),
                          control.predictor=list(compute=TRUE),
                          verbose=T,control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),
                          control.inla = list(int.strategy = 'eb',strategy="adaptive"))

summary(mod.malaria.zip.NI)
round(mod.malaria.zip.NI$summary.fixed,6)

##densidad betas
library(reshape2)
mf.ZIP.NI<-melt(mod.malaria.zip.NI$marginals.fixed)
cf.ZIP.NI <- spread(mf.ZIP.NI,Var2,value)
names(cf.ZIP.NI)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.ZIP.NI,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                   scales="free") + geom_vline(xintercept=0) + ylab("density")

##nuevo modelo sin acue,chicu,dengue, que resultaron ser no significativos
formula.zip.NI.2<- mala_total~1+valor+zika_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj,
    hyper=list(prec.unstruct=list(prior="logitbeta",param=c(0.5,0.5), initial=0),
    prec.spatial=list(prior="gaussian",param=c(0,1))))

mod.malaria.zip.NI.2<- inla(formula.zip.NI.2,family="zeroinflatedpoisson1",
                            data=datos, offset = log(E),
                            control.predictor=list(compute=TRUE),
                            verbose=T,control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),
                            control.inla = list(int.strategy = 'eb',strategy="adaptive"))

summary(mod.malaria.zip.NI.2)
round(mod.malaria.zip.NI.2$summary.fixed,6)

EXPBO.zip<-inla.emarginal(exp,mod.malaria.zip.NI.2$marginals.fixed[[1]])
EXPVAL.zip<-inla.emarginal(exp,mod.malaria.zip.NI.2$marginals.fixed[[2]])
expzika.zip<-inla.emarginal(exp,mod.malaria.zip.NI.2$marginals.fixed[[3]])
expbosq.zip<-inla.emarginal(exp,mod.malaria.zip.NI.2$marginals.fixed[[4]])
exppreci.zip<-inla.emarginal(exp,mod.malaria.zip.NI.2$marginals.fixed[[5]])
expalt.zip<-inla.emarginal(exp,mod.malaria.zip.NI.2$marginals.fixed[[6]])

round((c(EXPBO.zip,EXPVAL.zip,expzika.zip,expbosq.zip,exppreci.zip,expalt.zip)-1)*100,6)
print(c(EXPBO.zip,EXPVAL.zip,expzika.zip,expbosq.zip,exppreci.zip,expalt.zip))

##densidad betas
library(reshape2)
mf.ZIP.NI<-melt(mod.malaria.zip.NI.2$marginals.fixed)
cf.ZIP.NI <- spread(mf.ZIP.NI,Var2,value)
names(cf.ZIP.NI)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.ZIP.NI,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                        scales="free") + geom_vline(xintercept=0) + ylab("density")

############################################################################
############################################################################
##########################MAPA DE RIESGO RELATIVO###########################
############################################################################
############################################################################

MPIOS$RRZIP<- mod.malaria.zip.NI.2$summary.fitted.values[, "mean"]
writeOGR(MPIOS,"G:/","MPIOS", driver="ESRI Shapefile")
X11()
brks.p<-c(0,0.5,1,1.5,2,3,4,5,6,7,20,50,100,1000,6000)
tm_shape(MPIOS) + 
  tm_polygons("RRZIP",breaks=brks.p,title="R.RELATIVO ZIP" ,palette = "YlOrRd",title.size=0.5,
              contrast = 0.7, border.col = "gray30",id = "name")+
  tm_compass(type = "8star", position = c("left", "top")) +
  tm_legend(position = c("RIGTH", "TOP"),legend.format = list(text.to.columns=T))+
  tm_format_World() + 
  tm_style_gray(title = "Riesgo relativo de Malaria \npara cada municipio \nen Colombia \naño 2015",
                title.size=1)


                    #############################################
                    #############################################
                    #########ZIP INFORMATIVO#####################
                    #############################################
                    #############################################

formula.zip<- mala_total~1+valor+valor_1+deng_total+chic_total+zika_total+bosque_std + preci_std + alt_med_std +
  f(index, model="bym", graph=MPIOS.adj,
    hyper=list(prec.unstruct=list(prior="logitbeta",param=c(110.46824,62.09235)),
               prec.spatial=list(prior="gaussian",param=c(0,1))))

modelo.zip<- inla(formula.zip,family="zeroinflatedpoisson1",
                          data=datos, offset = log(E),
                          control.predictor=list(compute=TRUE),
                          verbose=T,control.compute=list(dic=TRUE,waic=TRUE,cpo=TRUE),
                          control.inla = list(int.strategy = 'eb',strategy="adaptive"))

summary(modelo.zip)
round(modelo.zip$summary.fixed,6)

ZIP.no.Informativa<-round(mod.malaria.zip.NI$summary.fixed,6)$mean*100
ZIP.Informativa<-round(modelo.zip$summary.fixed,6)$mean*100
variable<-c("B0","Alcantarillado","Acueducto","Dengue","Chicungunya","Zika","Bosque","Precipitación","Altura media")
data.frame(variable,ZIP.no.Informativa,ZIP.Informativa)
#####exponencial betas

EXPBO.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[1]])
EXPVAL.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[2]])
EXPVAL1.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[3]])
expdenge.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[4]])
expchicu.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[5]])
expzika.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[6]])
expbosq.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[7]])
exppreci.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[8]])
expalt.zip.inf<-inla.emarginal(exp,modelo.zip$marginals.fixed[[9]])


round((c(EXPBO.zip.inf,EXPVAL.zip.inf,EXPVAL1.zip.inf,expdenge.zip.inf,expchicu.zip.inf,expzika.zip.inf,expbosq.zip.inf,exppreci.zip.inf,expalt.zip.inf)-1)*100,6)
print(c(EXPBO.zip.inf,EXPVAL.zip.inf,EXPVAL1.zip.inf,expdenge.zip.inf,expchicu.zip.inf,expzika.zip.inf,expbosq.zip.inf,exppreci.zip.inf,expalt.zip.inf))
