#####INFORMACIÓN A PRIORI INFORMATIVA
Ipa_Col <- data.frame("AÑO" = 2000:2015, "IPA_MEDIO_ANUAL" = c(5.9,5.6,6.3,5,5,4.8,8.4,11.1,6.2,7.9,11.5,6.3,5.8,5,3.4,5.2))

library(xtable)
xtable(t(Ipa_Col))

row.names(Ipa_Col)<-2000:2015
var<-c("5.9","5.6","6.3","5","5","4.8","8.4",
       "11.1","6.2","7.9","11.5","6.3","5.8","5","3.4","5.2")
x11()
plot(Ipa_Col,ylab="IPA",main="IPA para Colombia 2000-2015",
     col="gray", cex=2.5)
text(Ipa_Col, row.names(Ipa_Col), cex=0.8, pos=3, col="red")
text(Ipa_Col, var, cex=0.8, pos=1, col="blue")

media=mean(Ipa_Col$IPA_MEDIO_ANUAL)
MargenError=qt(0.975,15)*sd(Ipa_Col$IPA_MEDIO_ANUAL)/(sqrt(15))
ExtrIzq=media-MargenError
ExtrDer=media+MargenError
print(c(ExtrIzq,media,ExtrDer))

library(maptools)

MPIOS<-readShapePoly("G:/TESIS/SHAPE/EPIDEMIOLOGIA")
datos<-as.data.frame(MPIOS@data)

library(sqldf)
pob_riesgo<-sqldf("Select nom_munici,total,alt_med
                  FROM datos
                  WHERE (alt_med+alt_std)<=1600")
pob_riesgo$total<-as.numeric(gsub(",", ".", gsub("\\.", "",pob_riesgo$total)))
Poblacion_total_riesgo<-sum(pob_riesgo$total)

Media_casos_confirmados<-Poblacion_total_riesgo*media/1000
LI_casos_confirmados<-Poblacion_total_riesgo*ExtrIzq/1000
LD_casos_confirmados<-Poblacion_total_riesgo*ExtrDer/1000
LI_lambda<-LI_casos_confirmados/1120
LD_lambda<-LD_casos_confirmados/1120
lambda<-Media_casos_confirmados/1120
print(c(LI_lambda,lambda,LD_lambda))
##############################
##############################
######priori para poisson#####
##############################
##############################
g<-function(a,b) (pgamma(108.69,a,b)-0.5)^2+(pgamma(129.3146,a,b)-pgamma(88.06544,a,b)-0.95)^2
gg<-function(ab) g(ab[1],ab[2])
minimizar<-nlm(gg,c(0.1,0.1))
alphaPoisson=minimizar$estimate[1]
betaPoisson=minimizar$estimate[2]
##############################
##############################
######priori para binomial####
##############################
##############################
pob_total<-sqldf("Select sum(total) as total
                  FROM datos
                  WHERE (alt_med+alt_std)<=1600")

Media_incidencia<-Media_casos_confirmados/pob_total
LD_incidencia<-LD_casos_confirmados/pob_total
print(c(Media_incidencia,LD_incidencia))

bOpt<-function(par) {sum((qbeta(c(0.5,0.95),par[1],par[2])-c(0.0064625,0.007688799))^2)}
bRes<-optim(c(1,2),bOpt,lower=c(0.1,0.1),control=list(parscale=c(0.1,1)))
bRes$par
qbeta(c(0.5,0.95),bRes$par[1],bRes$par[2])

xseq = seq(0,0.2,len=1001)
yseq = dbeta(xseq, bRes$par[1],bRes$par[2])
x11()
plot(xseq, yseq, type='l')
##############################
##############################
##a priori binomial negativa##
##############################
##############################
library(INLA)
optimiza<-function(lambda) {
  (inla.pc.pgamma(108.69,lambda)-0.5)^2+(inla.pc.pgamma(129.3146,lambda)-inla.pc.pgamma(88.06544,lambda)-0.95)^2
}
lambda.nb<-utils::str(nlm(optimiza, c(5)))
##############################
##############################
##############################
#######priori para ZIP########
##############################
##############################
##############################
library(readr)
malaria_incidents <- read_csv("G:/Datos/malaria_incidents.csv")
colombian_municipalities <- read_csv("G:/Datos/Exportados/colombian_municipalities1.csv")
library(dplyr)
colombian_municipalities <- mutate_all(colombian_municipalities, funs(toupper))
library(fastDummies) 
Malaria<-dummy_cols(malaria_incidents,select_columns = c("Ethnicity","Sex","Event"),remove_first_dummy = F)
View(Malaria)
library(sqldf)

Malaria2015<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2015
                   group by Municipio,Year") 

m2015<-count(Malaria2015)$n

Malaria2014<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2014
                   group by Municipio,Year") 
m2014<-count(Malaria2014)$n

Malaria2013<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2013
                   group by Municipio,Year") 
m2013<-count(Malaria2013)$n

Malaria2012<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2012
                   group by Municipio,Year") 
m2012<-count(Malaria2012)$n

Malaria2011<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2011
                   group by Municipio,Year") 
m2011<-count(Malaria2011)$n

Malaria2010<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2010
                   group by Municipio,Year") 
m2010<-count(Malaria2010)$n

Malaria2009<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2009
                   group by Municipio,Year") 
m2009<-count(Malaria2009)$n

Malaria2008<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2008
                   group by Municipio,Year") 
m2008<-count(Malaria2008)$n

Malaria2007<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM,
                   (sum(`Event_495 - MALARIA COMPLICADA`)+sum(`Event_490 - MALARIA VIVAX`)+sum(`Event_470 - MALARIA FALCIPARUM`)) AS TOTAL
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2007
                   group by Municipio,Year") 
m2007<-count(Malaria2007)$n

Ceros <- data.frame("AÑO" = 2007:2015, "Mpios_reportados" = c(m2007,m2008,m2009,m2010,m2011,m2012,m2013,m2014,m2015))
Ceros$porc<-1-Ceros$Mpios_reportados/1122
med<-mean(Ceros$porc)
MargenErrorzip=qt(0.975,8)*sd(Ceros$porc)/(sqrt(8))
ExtrIzqzip=med-MargenErrorzip
ExtrDerzip=med+MargenErrorzip
print(c(ExtrIzqzip,med,ExtrDerzip))

fun<-function(par) {sum((qbeta(c(0.5,0.95),par[1],par[2])-c(0.6407209,0.6991746))^2)}
res<-optim(c(1,2),fun,lower=c(0.1,0.1),control=list(parscale=c(0.1,1)))
res$par
qbeta(c(0.5,0.95),res$par[1],res$par[2])

xseq = seq(0,1,len=1001)
yseq = dbeta(xseq, res$par[1],res$par[2])
x11()
plot(xseq, yseq, type='l')
