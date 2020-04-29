library(maptools)
library(dplyr)
library(SpatialEpi)  ##Valor esperado
library(sqldf)       ##Consultas sql
library(classInt)
library(RColorBrewer)
library(imager)
library(raster)

MPIOS<-readShapePoly("G:/Shapefiles/Epidemiologia/EPIDEMIOLOGIA")
datos<-as.data.frame(MPIOS@data)

############################################################################
###CALCULO DEL VALOR ESPERADO MEDIANTE LA ESTRATIFICACION POR GRUPO ÉTNICO##
###AÚN NO ES FUNCIONAL DADO QUE NO SE TIENEN LA POBLACIÓN ESPECIFICA A 2015#
###SIN EMBARGO, SE TENDRÁ EN CUENTA SI MÁS A DELANTE SE LLEGASEN A OBTENER##
##LOS DATOS
############################################################################

datos$total<-as.numeric(as.character(datos$total))
datos$otros<-datos$total-(datos$Indigenas+datos$Afros+datos$Raizal+datos$rom+datos$palenquero)
POB.INDI<-select(datos,COD_DANE,NOM_MUNICI,NOM_DEPART,Indigenas,Mala_Indig)
POB.INDI$RAZA<-"IND"
colnames(POB.INDI)[4] <- "POB"
colnames(POB.INDI)[5] <- "CASOS"
POB.AFRO<-select(datos,COD_DANE,NOM_MUNICI,NOM_DEPART,Afros,Mala_Afro)
POB.AFRO$RAZA<-"AFRO"
colnames(POB.AFRO)[4] <- "POB"
colnames(POB.AFRO)[5] <- "CASOS"
POB.RAIZAL<-select(datos,COD_DANE,NOM_MUNICI,NOM_DEPART,Raizal,Mala_Raiza)
POB.RAIZAL$RAZA<-"RAIZA"
colnames(POB.RAIZAL)[4] <- "POB"
colnames(POB.RAIZAL)[5] <- "CASOS"
POB.ROM<-select(datos,COD_DANE,NOM_MUNICI,NOM_DEPART,rom,Mala_Roman)
POB.ROM$RAZA<-"ROM"
colnames(POB.ROM)[4] <- "POB"
colnames(POB.ROM)[5] <- "CASOS"
POB.PALE<-select(datos,COD_DANE,NOM_MUNICI,NOM_DEPART,palenquero,Mala_Palen)
POB.PALE$RAZA<-"PALE"
colnames(POB.PALE)[4] <- "POB"
colnames(POB.PALE)[5] <- "CASOS"
POB.OTROS<-select(datos,COD_DANE,NOM_MUNICI,NOM_DEPART,otros,Mala_otros)
POB.OTROS$RAZA<-"OTRO"
colnames(POB.OTROS)[4] <- "POB"
colnames(POB.OTROS)[5] <- "CASOS"

POBLACION<-rbind(POB.INDI,POB.AFRO,POB.RAIZAL,POB.ROM,POB.PALE,POB.OTROS)
POBLACION<-POBLACION[order(POBLACION$NOM_MUNICI), ]

E <- expected(POBLACION$POB, POBLACION$CASOS, 6)
##AQUI SE OBSERVA EN LA SIGUIENTE CONSULTA LOS 60 CASOS DONDE LA POBLACION
##ES MENOR A LOS CASOS REPORTADOS, POR ENDE NO SE PUEDE USAR LA POBLACION A 2005

pob_Select<-sqldf("SELECT COD_DANE,NOM_MUNICI,NOM_DEPART,POB,CASOS,RAZA
                   FROM POBLACION 
                   WHERE POB<CASOS ")
View(pob_Select)

##########################################################################
##########################################################################
##########################################################################
###CALCULO DEL VALOR ESPERADO MEDIANTE LA ESTRATIFICACION POR SEXO########
###ES FUNCIONAL, SIN EMBARGO, MUY SESGADA A LA REALIDAD DE LA ENFERMEDAD##
##########################################################################
##########################################################################
##########################################################################

POB.HOMB<-select(datos,cod_dane,nom_munici,nom_depart,pob_hombre,sex_mascul)
POB.HOMB$SEX<-"HOMBRE"
colnames(POB.HOMB)[4] <- "POB"
colnames(POB.HOMB)[5] <- "CASOS"

POB.MUJ<-select(datos,cod_dane,nom_munici,nom_depart,pob_mujer,sex_femeni)
POB.MUJ$SEX<-"MUJER"
colnames(POB.MUJ)[4] <- "POB"
colnames(POB.MUJ)[5] <- "CASOS"

POBLACION<-rbind(POB.HOMB,POB.MUJ)

E <- expected(POBLACION$POB, POBLACION$CASOS, 2)
MPIOS$E <-E

###Cálculo del SIR
SIR<-MPIOS$mala_total/MPIOS$E
MPIOS$SIR<-SIR

View(MPIOS@data)
            ####################################
            ####################################
            ####################################
            ###########MAPA DE SIR##############
            ####################################
            ####################################
            ####################################
X11()
colores1 <- brewer.pal(9, "YlOrRd")
brks.sir<-c(0,0.2,0.5,1,10,50,100,150,200,300)
rangos1 <- leglabs(as.vector(round(brks.sir, digit = 2)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(MPIOS$SIR, brks.sir,all.inside=F,rightmost.closed = T)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["SIR"], axes = TRUE,cex.axis=0.8,main="SIR para el año 2015 en Colombia",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)",border="gray75")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.5, 14, -78, 5.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("topleft", title=expression("SIR 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)
arrows( -76.2,5.5, -80,4.3)
text(-80,4.3,"Chocó",pos = 1,cex = 1.2)
text(-80,3.7,"Bagadó",pos = 1,cex = 1.2)
arrows( -69.5,4.3, -65,3)
text(-65,3,"Vichada",pos = 1,cex = 1)
text(-65,2.5,"Cumaribo",pos = 1,cex = 1)
arrows( -73.7,5.1, -68,8)
text(-68,9.5,"Cundinamarca",pos = 1,cex = 1)
text(-68,9,"Chocontá",pos = 1,cex = 1)
arrows( -70,-3.6, -80,-1.5)
text(-80,0,"Amazonas",pos = 1,cex = 1)
text(-80,-0.5,"Leticia",pos = 1,cex = 1)

datos<-as.data.frame(MPIOS@data)
consulta<-sqldf("SELECT cod_dane,nom_munici,mala_total,E,SIR
          FROM datos 
          WHERE cod_dane='91001' OR cod_dane='27073' OR cod_dane='25183' OR cod_dane='99773'
          group by nom_depart") 

xtable(consulta)