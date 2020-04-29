##########################################################################
################## MUNICIPIOS DE COLOMBIA ################################
##LOS DATASETS GENERADOS FUERON ENLAZADOS AL SHAPEFILE MEDIANTE UN SIG####
##########################################################################
library(maptools)
library(classInt)
library(RColorBrewer)
library(imager)
library(raster)
library(ggplot2)

MPIOS<-readShapePoly("G:/Shapefiles/Epidemiologia/EPIDEMIOLOGIA")
datos<-as.data.frame(MPIOS@data)
View(datos)
names(datos)
Escudo<-load.image("G:/Imagenes/Escudo.JPG")
##MAPA DE COLOMBIA
##mis.colores <- colorRampPalette(c("yellow", "red", "green","brown3","chocolate3","burlywood3","chartreuse3","coral3","darkgoldenrod3","darkolivegreen3","darkorchid3"))
X11()
plot(MPIOS["nom_munici"], axes = TRUE,cex.axis=0.8,main="República de Colombia",col=terrain.colors(32),xlab="Longitud (G°)",ylab="Latitud (G°)")
rasterImage(Escudo,-84,7,-78.5,12)
rect(-83.5, 11.8, -78.5, 3.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
text(-81,6.8,"Facultad de Ingeniería",cex=0.65)
text(-81,6.2,"Ingeniería catastral y",cex=0.65)
text(-81,5.9,"geodesia",cex=0.65)
text(-81,5.1,"Municipios de Colombia",cex=0.55)
text(-81,4.7,"Sistema de referencia: WGS84",cex=0.45)
text(-81,4.3,"CRS: 4386",cex=0.45)
text(-81,3.9,"Unidades: GMS",cex=0.45)
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow() 
############################################################################
##############ANÁLISIS EXPLORATORIO DE DATOS ESPACIALES#####################
############################################################################
######################ESTADÍSTICAS DESCRIPTIVAS
##Malaria 2015
x11()
ggplot(data=datos, aes(datos$mala_total)) +  
  geom_histogram(breaks=seq(0, 5722, by = 50),fill="darkblue",col="black") + 
  labs(x="Casos registrados", y="Municipios")

X11()
var<-MPIOS$mala_total
colores <- brewer.pal(8, "YlOrRd")
brks<-c(0,50,150,300,1000,2000,5000,Inf)
rangos <- leglabs(as.vector(round(brks, digit = 0)),under="Menos de",
                  over="Más de")
codigos_num <- findInterval(var, brks,all.inside=TRUE)
codigos_color <- colores[codigos_num]
plot(MPIOS["mala_total"], axes = TRUE,cex.axis=0.8,main="Casos reportados de Malaria para el año 2015 en Colombia",col=codigos_color,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.5, 8, -79.5, 1.3, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("Malaria 2015") ,
       legend=rangos, fill=colores, horiz=FALSE,
       box.col = NA, cex = 1.0)


##Dengue 2015
x11()
ggplot(data=datos, aes(datos$deng_total)) +  
  geom_histogram(breaks=seq(0, 14535, by = 200),fill="darkblue",col="black") + 
  labs(x="Casos registrados", y="Municipios")

X11()
var1<-MPIOS$deng_total
colores1 <- brewer.pal(9, "YlOrRd")
brks1<-c(0,50,150,300,1000,2000,5000,10000,Inf)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, brks1,all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["deng_total"], axes = TRUE,cex.axis=0.8,main="Casos reportados de Dengue para el año 2015 en Colombia",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.5, 9, -79, 0.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("Dengue 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)


##Zika 2015
x11()
ggplot(data=datos, aes(datos$zika_total)) +  
  geom_histogram(breaks=seq(0, 1500, by = 100),fill="darkblue",col="black") + 
  labs(x="Casos registrados", y="Municipios")

X11()
var1<-MPIOS$zika_total
colores1 <- brewer.pal(7, "YlOrRd")
brks1<-c(0,50,150,300,600,1000,Inf)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, brks1,all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["zika_total"], axes = TRUE,cex.axis=0.8,main="Casos reportados de Zika para el año 2015 en Colombia",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.5, 9, -79, 0.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("Zika 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)

##chicungunya 2015
x11()
ggplot(data=datos, aes(datos$chic_total)) +  
  geom_histogram(breaks=seq(0, 700, by = 50),fill="darkblue",col="black") + 
  labs(x="Casos registrados", y="Municipios")

X11()
var1<-MPIOS$chic_total
colores1 <- brewer.pal(7, "YlOrRd")
brks1<-c(0,50,100,200,300,600,Inf)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, brks1,all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["chic_total"], axes = TRUE,cex.axis=0.8,main="Casos reportados de Chikungunya para el año 2015 en Colombia",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.8, 9, -79, 0.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("Chikungunya 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)

##Alcantarillado 2015
x11()
ggplot(data=datos, aes(datos$valor)) +  
  geom_histogram(breaks=seq(0, 100, by = 10),fill="darkblue",col="black") + 
  labs(x="Porcentaje", y="Conteo de Municipios")

##Acueducto 2015
x11()
ggplot(data=datos, aes(datos$valor_1)) +  
  geom_histogram(breaks=seq(0, 100, by = 10),fill="darkblue",col="black") + 
  labs(x="Porcentaje", y="Conteo de Municipios")

x11()
old.par <- par(mfrow=c(1, 2))
var1<-MPIOS$valor
var2<-MPIOS$valor_1
colores1 <- brewer.pal(9, "YlOrRd")
brks1<-c(10,20,30,40,50,60,70,80,90)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, sort(brks1),all.inside=TRUE)
codigos_num2 <- findInterval(var2, sort(brks1),all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
codigos_color2 <- colores1[codigos_num2]
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de %",
                   over="Más de %")
plot(MPIOS["valor"], axes = T,cex.axis=0.8,col=codigos_color1)
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
legend("left", title=expression("Cobertura Alcantarillado 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 0.7)
plot(MPIOS["valor_1"], axes = T,cex.axis=0.8,col=codigos_color2)
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
legend("left", title=expression("Cobertura Acueducto 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 0.7)
par(old.par)


##Altura media 2015
x11()
ggplot(data=datos, aes(datos$alt_med)) +  
  geom_histogram(breaks=seq(0, 3800, by = 150),fill="darkblue",col="black") + 
  labs(x="Altura m.s.n.m.", y="Municipios")


X11()
var1<-MPIOS$alt_med
colores1 <- brewer.pal(9, "RdYlBu")
brks1<-c(0,100,200,500,1000,1500,2000,2500,Inf)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, brks1,all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["chic_total"], axes = TRUE,cex.axis=0.8,main="Altura media sobre el nivel del mar en Colombia",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.8, 9, -79, 0.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("Altura m.s.n.m. 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)

##Bosque 2015
x11()
ggplot(data=datos, aes(datos$bosque_bos)) +  
  geom_histogram(breaks=seq(0, 100, by = 10),fill="darkblue",col="black") + 
  labs(x="Porcentaje", y="Municipios")

X11()
var1<-MPIOS$bosque_bos
colores1 <- brewer.pal(9, "Greens")
brks1<-c(10,20,30,40,50,60,70,80,Inf)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, brks1,all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["chic_total"], axes = TRUE,cex.axis=0.8,main="Procentaje de bosque en Colombia para el año 2015",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.8, 9, -79, 0.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("% Bosque 2015") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)

##Precipitación 2015
x11()
ggplot(data=datos, aes(datos$p_preci_me)) +  
  geom_histogram(breaks=seq(0, 8200, by = 200),fill="darkblue",col="black") + 
  labs(x="Precipitación promedio(mm)", y="Municipios")

X11()
var1<-MPIOS$p_preci_me
colores1 <- brewer.pal(9, "Blues")
brks1<-c(500,1000,1500,2000,2500,3000,4000,5000,Inf)
rangos1 <- leglabs(as.vector(round(brks1, digit = 0)),under="Menos de",
                   over="Más de")
codigos_num1 <- findInterval(var1, brks1,all.inside=TRUE)
codigos_color1 <- colores1[codigos_num1]
plot(MPIOS["chic_total"], axes = TRUE,cex.axis=0.8,main="Promedio de precipitación en Colombia para el año 2015",col=codigos_color1,xlab="Longitud (G°)",ylab="Latitud (G°)")
scalebar(1000, type='bar', divs=4,below="Kilométros")
addnortharrow()
rect(-84.8, 9, -79, 0.5, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"))
legend("left", title=expression("Precipitación (mm)") ,
       legend=rangos1, fill=colores1, horiz=FALSE,
       box.col = NA, cex = 1.0)
