#############################################
##################LIBRERIAS##################
#############################################
library(readr)       ##csv
library(readxl)      ##Excel
library(dplyr)
library(sqldf)       ##SQL
library(fastDummies) ##Dummys
library(tidyr)       ##Funtion unite in dengue

MUNICIPIOS <- read_csv("/Datos/MUNICIPIOS.csv")
MUNICIPIOS<-select(MUNICIPIOS,MunicipalityID,NOM_DEPART,NOM_MUNICI,COD_DANE)
names(MUNICIPIOS)[1]<-"IDMunicipality"
colombian_municipalities <- read_csv("G:/Datos/colombian_municipalities.csv")
colombian_municipalities <- mutate_all(colombian_municipalities, funs(toupper))

colombian_municipalities1<-sqldf("SELECT lon,lat,MunicipalityID,COD_DANE,`Department Name`,`Municipality Name`,
          `Rural Population`,`Urban Population`,`Total Population`
          FROM MUNICIPIOS, colombian_municipalities 
          WHERE colombian_municipalities.MunicipalityID=MUNICIPIOS.IDMunicipality
          group by NOM_MUNICI") 
View(colombian_municipalities1)
write.csv(colombian_municipalities1, "G:/Datos/Exportados/colombian_municipalities1.csv")
#######################################################
##################POBLACION A?O 2015###################
#################### URBANO Y RURAL ###################
#######################################################
#######################################################

Municipal_area_1985_2020 <- read_excel("G:/Datos/Municipal_area_1985-2020.xls", 
                                       sheet = "Municipios")
Municipal_area_1985_2020[632,]
POB<-select(Municipal_area_1985_2020,...2,...4,...15,...31,...47)
POB<-POB[5:1128,]
names(POB)[names(POB) == "...2"] <- "DEPARTAMENTO"
names(POB)[names(POB) == "...4"] <- "MUNICIPIO"
names(POB)[names(POB) == "...15"] <- "TOTAL"
names(POB)[names(POB) == "...31"] <- "URBANO"
names(POB)[names(POB) == "...47"] <- "RURAL"
POB <- mutate_all(POB, funs(toupper))

##PRUE<-iconv(POB$MUNICIPIO,from="UTF-8",to="ASCII//TRANSLIT") ELIMINAR TILDES
PRUE<-gsub("(CD)","Cor. Departamental",POB$MUNICIPIO)
POB$MUNICIPIO<-PRUE
View(POB)
write.csv(POB, "G:/Datos/Exportados/POBLACION.csv")
################################################################
################## POBLACION ETNICA A?O 2005 ###################
################################################################
################################################################
pobEtnica2005 <- read_excel("G:/Datos/pobEtnica2005.xlsx")
View(pobEtnica2005)

INDIGENA<-sqldf("SELECT Departamento, Entidad AS Municipio, Subcategor?a,
      Indicador, `Dato Num?rico` AS Indigenas
      FROM pobEtnica2005
      WHERE Indicador='Poblaci?n ind?gena'
      group by Municipio")
AFRO<-sqldf("SELECT Departamento AS Depto, Entidad AS Municipio1, Subcategor?a AS Subcategor?a1,
      Indicador AS Ind1, `Dato Num?rico` AS Afros
      FROM pobEtnica2005
      WHERE Indicador='Poblaci?n negra, mulata o afrocolombiana'
      group by Municipio1")
RAIZAL<-sqldf("SELECT Departamento AS Depto1, Entidad AS Municipio2, Subcategor?a AS Subcategor?a2,
      Indicador AS Ind2, `Dato Num?rico` AS Raizal
      FROM pobEtnica2005
      WHERE Indicador='Poblaci?n raizal'
      group by Municipio2")
RrOM<-sqldf("SELECT Departamento AS Depto2, Entidad AS Municipio3, Subcategor?a AS Subcategor?a3,
      Indicador AS Ind3, `Dato Num?rico` AS rom
      FROM pobEtnica2005
      WHERE Indicador='Poblaci?n rom'
      group by Municipio3")
PALENQUERO<-sqldf("SELECT Departamento AS Depto3, Entidad AS Municipio4, Subcategor?a AS Subcategor?a3,
      Indicador AS Ind4, `Dato Num?rico` AS palenquero
      FROM pobEtnica2005
      WHERE Indicador='Poblaci?n palenquero'
      group by Municipio4")

PobEtnica_2005<-sqldf("SELECT Departamento,
      Municipio,Indigenas,Afros,Raizal,rom,palenquero
      FROM INDIGENA,AFRO,RAIZAL,RrOM,PALENQUERO
      WHERE INDIGENA.Municipio=AFRO.Municipio1 AND INDIGENA.Municipio=RAIZAL.Municipio2 AND
      INDIGENA.Municipio=RrOM.Municipio3 AND INDIGENA.Municipio=PALENQUERO.Municipio4
      group by Municipio")
PobEtnica_2005$Indigenas<-as.numeric(gsub(",", ".", gsub("\\.", "", PobEtnica_2005$Indigenas)))
PobEtnica_2005$Afros<-as.numeric(gsub(",", ".", gsub("\\.", "", PobEtnica_2005$Afros)))
PobEtnica_2005$Raizal<-as.numeric(gsub(",", ".", gsub("\\.", "", PobEtnica_2005$Raizal)))
PobEtnica_2005$rom<-as.numeric(gsub(",", ".", gsub("\\.", "", PobEtnica_2005$rom)))
PobEtnica_2005$palenquero<-as.numeric(gsub(",", ".", gsub("\\.", "", PobEtnica_2005$palenquero)))

PobEtnica_2005 <- mutate_all(PobEtnica_2005, funs(toupper))

write.csv(PobEtnica_2005, "G:/Datos/PobEtnica_2005.csv")
#######################################################
################## MALARIA A?O 2015 ###################
#######################################################
#######################################################
malaria_incidents <- read_csv("G:/Datos/malaria_incidents.csv")

###Creacion dummys

Malaria<-dummy_cols(malaria_incidents,select_columns = c("Ethnicity","Sex","Event"),remove_first_dummy = F)
View(Malaria)

###MALARIA PARA EL A?O 2015

Malaria2015<-sqldf("SELECT COD_DANE AS COD_DANE1,Department,`Municipality Name` AS Municipio,Year,
                   sum(Ethnicity_Indigenous) as Mala_Indigenas,
                   sum(Ethnicity_Other) as Mala_otros,sum(Ethnicity_Afro) as Mala_Afro,
                   sum(Ethnicity_Raizal) as Mala_Raizal,sum(Ethnicity_Romani) as Mala_Romani,
                   sum(Ethnicity_Palenquero) as Mala_Palenquero,sum(Sex_MALE) as Sex_Masculino,
                   sum(Sex_FEMALE) as Sex_Femenino,sum(`Sex_NR - NO REPORTADO`) as Sex_NOReportado,
                   sum(`Sex_NO DEFINIDO`) as Sex_NODefinido,sum(`Event_495 - MALARIA COMPLICADA`) as MALARIA_COMPLICADA,
                   sum(`Event_490 - MALARIA VIVAX`) as MALARIA_VIVAX,sum(`Event_470 - MALARIA FALCIPARUM`) as MALARIA_FALCIPARUM
                   FROM Malaria, colombian_municipalities 
                   ON Malaria.MunicipalityID=colombian_municipalities.MunicipalityID AND Year=2015
                   group by Municipio,Year") 

Malaria2015 <- mutate_all(Malaria2015, funs(toupper))
View(Malaria2015)


write.csv(Malaria2015, "G:/Datos/Malaria2015.csv")

#######################################################
################### DENGUE A?O 2015 ###################
#######################################################
#######################################################


Dengue_nominal2015 <- read_excel("G:/Datos/Dengue_nominal.xlsx",sheet = "2015")
Dengue_nominal2015<-unite(Dengue_nominal2015,COD_DANE,c(7:8),  sep = "")
Dengue_select<-sqldf("SELECT Evento,ANO,COD_DANE AS COD_DANE2,Departamento,Municipio, count(Municipio) as Dengue_Total 
                      FROM Dengue_nominal2015 group by Municipio, ANO")
Dengue_select$COD_DANE2<-as.numeric(Dengue_select$COD_DANE2)
View(Dengue_select)
write.csv(Dengue_select, "G:/Datos/Dengue2015.csv")
###########################################################
################## CHICUNGUNYA A?O 2015 ###################
###########################################################
###########################################################
Chikungunya <- read_excel("G:/Datos/Chikungunya.xlsx")
Chikungunya$a?o<-format(as.Date(Chikungunya$fec_not, format="%Y/%m/%d"),"%Y")

Chikungunya<-select(Chikungunya,cod_eve,a?o,hombres,mujeres,`suma sex`,CODIGO,nmun_notif,ndep_notif)
View(Chikungunya)
write.csv(Chikungunya, "G:/Datos/Chikungunya.csv")

Chikungunya_select<-sqldf("SELECT cod_eve as COD_EVE,a?o as ANO,
ndep_notif as Departamento,CODIGO AS COD_DANE3,
CASE
  WHEN ndep_notif = 'BOGOTA' THEN 'BOGOTA'
  ELSE nmun_notif
  END AS Municipio,
sum(hombres) as Hombres,sum(mujeres) as Mujeres,sum(`suma sex`) as Chikungunya_Total
FROM Chikungunya WHERE ANO=2015 
group by Municipio, ANO")
Chikungunya_select$COD_DANE3<-as.numeric(Chikungunya_select$COD_DANE3)

View(Chikungunya_select)

write.csv(Chikungunya_select, "G:/Datos/Chikungunya2015.csv")
####################################################
################## ZIKA A?O 2015 ###################
####################################################
####################################################

Zika <- read_excel("G:/Datos/Zika.xlsx")
View(Zika)
Zika<-select(Zika,COD_EVE,ANO,cod_mun,Departamento,Municipio,conteo)
write.csv(Zika, "G:/Datos/Zika.csv")

Zika_Select<-sqldf("SELECT COD_EVE,ANO,cod_mun AS COD_DANE4,Departamento,Municipio, sum(conteo) as Zika_Total 
                   FROM Zika group by Municipio, ANO")
Zika_Select$COD_DANE4<-as.numeric(Zika_Select$COD_DANE4)
View(Zika_Select) 
write.csv(Zika_Select, "G:/Datos/Zika2015.csv")

##################################################################
################## DATASETS GENERADOS ############################
##################################################################
##################################################################
POB                 ##POBLACION.csv
PobEtnica_2005      ##PobEtnica_2005.csv
Malaria2015         ##Malaria2015.csv
Dengue_select       ##Dengue2015.csv
Chikungunya_select  ##Chikungunya2015.csv
Zika_Select         ##Zika2015.csv
##########################################################################
################## MUNICIPIOS DE COLOMBIA ################################
##LOS DATASETS GENERADOS FUERON ENLAZADOS AL SHAPEFILE MEDIANTE UN SIG####
##########################################################################
