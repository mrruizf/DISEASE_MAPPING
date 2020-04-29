library(maptools)
library(spdep)       ##Matriz de contiguidad
library(adespatial)
library(xtable)
########################################################################
######################MATRIZ DE PESOS ESPACIALES
MPIOS<-readShapePoly("G:/Shapefiles/Epidemiologia/EPIDEMIOLOGIA")

##vecinos
towwer <- poly2nb(MPIOS, queen=F)            # Torre
towwer6<-nblag(towwer,6)
towwer<-nb2listw(towwer, style="B", glist = NULL)
towwer2<-nb2listw(towwer6[[2]], style="B", glist = NULL)
towwer3 <-nb2listw(towwer6[[3]], style="B", glist = NULL)
towwer4 <-nb2listw(towwer6[[4]], style="B", glist = NULL)
towwer5 <-nb2listw(towwer6[[5]], style="B", glist = NULL)
towwer6 <-nb2listw(towwer6[[6]], style="B", glist = NULL)


queen <- poly2nb(MPIOS, queen=T)              #reina
queen6 <- nblag(queen,6)
queen<-nb2listw(queen, style="B", glist = NULL)
queen2<-nb2listw(queen6[[2]], style="B", glist = NULL)
queen3 <-nb2listw(queen6[[3]], style="B", glist = NULL)
queen4 <-nb2listw(queen6[[4]], style="B", glist = NULL)
queen5 <-nb2listw(queen6[[5]], style="B", glist = NULL)
queen6 <-nb2listw(queen6[[6]], style="B", glist = NULL)

coordenadas<-coordinates(MPIOS)    #k-vecinos
k1 <- knn2nb(knearneigh(coordenadas))
k1 <-nb2listw(k1, style="B", glist = NULL)
k2 <- knn2nb(knearneigh(coordenadas,2))
k2 <-nb2listw(k2, style="B", glist = NULL)
k3 <- knn2nb(knearneigh(coordenadas,3))
k3 <-nb2listw(k3, style="B", glist = NULL)
k4 <- knn2nb(knearneigh(coordenadas,4))
k4 <-nb2listw(k4, style="B", glist = NULL)
k5 <- knn2nb(knearneigh(coordenadas,5))
k5 <-nb2listw(k5, style="B", glist = NULL)
k6 <- knn2nb(knearneigh(coordenadas,6))
k6 <-nb2listw(k6, style="B", glist = NULL)

#Distancia de Gabriel 
gabrielnb=graph2nb(gabrielneigh(coordenadas),sym=TRUE)
gabriel <-nb2listw(gabrielnb, style="B", glist = NULL)
x11()
plot(MPIOS,border="gray")
plot(gabrielnb,coordenadas,add=T,col="red")
title(main="Gráfica de Gabriel")

#Triangulación Delaunay
trinb=tri2nb(coordenadas)
delaunay <-nb2listw(trinb, style="B", glist = NULL)

X11()
plot(MPIOS,border="gray")
plot(trinb,coordenadas,add=T,col="blue")
title(main="Triangulación Delaunay")

########################################################################
#################SELECCIÓN DE LA MATRIZ DE PESOS########################

Pesos.list<-list(reina1=queen,reina2=queen2,reina3=queen3,reina4=queen4,reina5=queen5,reina6=queen6,
                 torre1=towwer,torre2=towwer2,torre3=towwer3,torre4=towwer4,torre5=towwer5,torre6=towwer6,
                 kvecinos1=k1,kvecinos2=k2,kvecinos3=k3,kvecinos4=k4,kvecinos5=k5,kvecinos6=k6,gabriel=gabriel,delaunay=delaunay)

class(Pesos.list)
nbw <- length(Pesos.list)
1 - (1 - 0.05)^(nbw)
W_sel <- listw.select(datos$mala_total, Pesos.list, MEM.autocor = "all", 
                      p.adjust = TRUE, nperm = 50)
W_sel$candidates
W_sel$best.id

xtable(W_sel$candidates, digits = 6)
W_sel$best$MEM.select
Best.S.W.M<-Pesos.list[W_sel$best.id]