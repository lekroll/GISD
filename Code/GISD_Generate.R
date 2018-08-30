# GISD - German Index of Socio-Economic Deprivation
# Author: Lars Eric Kroll, Robert Koch Institut, Berlin
# Citation: Kroll LE, Schumann M, Hoebel J et al. (2017) Regional health differences – developing a socioeconomic deprivation 
#                   index for Germany. Journal of Health Monitoring 2(2):98–114. DOI 10.17886/RKI-GBE-2017-048ISSN 2511-2708

# This is an R Reproduction of the Original Index, using updated Data

# Librarys
library(tidyr)
library(dplyr)
library(readxl)

# Working Directory
setwd("S:/OE/FG28/205 Regionale Unterschiede/GISD/")

# Reference Dataset
Referenz_1998_2014 <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "Gemeinden", na = "NA", skip = 1)
names(Referenz_1998_2014)[8] <-"Stadt-/Gemeindetyp label"
Referenz_1998_2014[11] <- NULL
Referenz_1998_2014[11] <- NULL
names(Referenz_1998_2014)[9] <- "Mittelbereich Nr"
names(Referenz_1998_2014)[10] <- "Mittelbereich"

Kreise_1998_2014 <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "Kreise", skip = 1)
Kreise_1998_2014[2] <-NULL
names(Kreise_1998_2014)[2] <- "Kreis: Fläche in km²"
names(Kreise_1998_2014)[3] <- "Kreis: Bevölkerung"
names(Kreise_1998_2014)[4] <- "Bundesland Nr"
names(Kreise_1998_2014)[5] <- "Bundesland"
Kreise_1998_2014[6] <-NULL
names(Kreise_1998_2014)[6] <- "West/Ost"
names(Kreise_1998_2014)[11] <- "Siedlungsstruktureller Kreistyp Nr"
names(Kreise_1998_2014)[12] <- "Siedlungsstruktureller Kreistyp"
names(Kreise_1998_2014)[13] <- "Städtischer/Ländlicher Raum Nr"
names(Kreise_1998_2014)[14] <- "Städtischer/Ländlicher Raum"
names(Kreise_1998_2014)[15] <- "Raumordnungsregion Nr"
names(Kreise_1998_2014)[16] <- "Raumordnungsregion"
names(Kreise_1998_2014)[17] <- "Siedlungsstruktureller Regionstyp Nr"
names(Kreise_1998_2014)[18] <- "Siedlungsstruktureller Regionstyp"
names(Kreise_1998_2014)[19] <- "Arbeitsmarktregion Nr"
names(Kreise_1998_2014)[20] <- "Arbeitsmarktregion"
names(Kreise_1998_2014)[21] <- "Regierungsbezirk Nr"
names(Kreise_1998_2014)[22] <- "Regierungsbezirk"
names(Kreise_1998_2014)[23] <- "NUTS2"
names(Kreise_1998_2014)[24] <- "NUTS2 Name"

# Basedatasetz (Kreiskennziffern)
Basedata    <- as.data.frame(Kreise_1998_2014$Kreiskennziffer/1000)
names(Basedata)[1] <- "Kennziffer"
Basedata$Jahr <- 2014

# Load INKAR datasets
inputdataset <- list.files("Data/INKAR_1998_2014")
for(file in inputdataset){
  myimport <- read_excel(paste0("Data/INKAR_1998_2014/",file), skip = 1, sheet = "Daten", col_types = c("text"))
  indicator  <- unlist(strsplit(unlist(strsplit(file,"_"))[2],"[.]"))[1]
  names(myimport)[1] <- "Kennziffer"
  myimport[2] <- NULL
  names(myimport[2]) <- "Art"
  myimport[2] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T)
  names(myimport)[3] <- indicator
  myimport$Kennziffer<-as.numeric(myimport$Kennziffer)
  Basedata <- full_join(Basedata,myimport,by=c("Kennziffer","Jahr"))
  rm(indicator,myimport,file)
  }
rm(inputdataset)

# Trennen von Daten nach vorliegenender Ebene (NOTE: ELEGANTER NACH myimport$Art???)
Basedata_Gemeindeverbandsebene <- Basedata %>% select(Kennziffer,Jahr,Arbeitslosigkeit,Beschäftigtenquote,Einkommenssteuer)
Basedata_Kreisebene <- Basedata %>% select(-Arbeitslosigkeit,-Beschäftigtenquote,-Einkommenssteuer)

# Ergebnisdatensatz (alle Gemeinden und alle Jahre mit Daten)
Basedata <- as.data.frame(expand.grid(Gemeindekennziffer=Referenz_1998_2014$Gemeindekennziffer,Jahr=seq(min(Basedata$Jahr):max(Basedata$Jahr))+min(Basedata$Jahr)-1))
Basedata <- left_join(Basedata,Referenz_1998_2014,by=c("Gemeindekennziffer"))
Basedata$Kreiskennziffer <- as.numeric(Basedata$Kreiskennziffer)/1000
Basedata <- left_join(Basedata,Basedata_Kreisebene,by=c("Kreiskennziffer"="Kennziffer","Jahr"))
Basedata <- left_join(Basedata,Basedata_Gemeindeverbandsebene,by=c("Kennziffer Gemeindeverband"="Kennziffer","Jahr"))
rm(Basedata_Gemeindeverbandsebene,Basedata_Kreisebene)

# Fehlende Werte imputieren (hier fortfahren)
Basedata <-Basedata %>% filter(Jahr>=1998)

# Faktorenanalyse

# Ergebnisausgabe nach Ebene

