# GISD - German Index of Socio-Economic Deprivation
# Author: Lars Eric Kroll, Robert Koch Institut, Berlin
# Citation: Kroll LE, Schumann M, Hoebel J et al. (2017) Regional health differences â€“ developing a socioeconomic deprivation 
#           index for Germany. Journal of Health Monitoring 2(2):98â€“114. DOI 10.17886/RKI-GBE-2017-048ISSN 2511-2708

# Revision: 2018.v2
# Date: 2018-03-22

# Librarys
require("tidyverse") # Tidyverse Methods
require("readxl") # Read Excel
require("imputeTS") # Impute Missing Features
require("haven") # write Stata-dta
require("rgeos") # intersection
require("rgdal") # read-shapefiles, transform Projections
require("raster") # shape file intersections


# Working Directory
mypath <- "C:/Users/krolll/Desktop/GISD"
setwd(mypath)

# Create Output directories if necessary
dir.create("Revisions")
dir.create("Revisions/2018")
dir.create("Revisions/2018/Bund")

# Import data
Referenz_1998_2014 <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "Gemeinden", na = "NA", skip = 1)
names(Referenz_1998_2014)[8] <-"Stadt-/Gemeindetyp label"
Referenz_1998_2014[11] <- NULL
Referenz_1998_2014[11] <- NULL
names(Referenz_1998_2014)[9] <- "Mittelbereich Nr"
names(Referenz_1998_2014)[10] <- "Mittelbereich"

Kreise_1998_2014 <- read_excel("Data/Referenz/Referenz_1998_2014.xlsx", sheet = "Kreise", skip = 1)
Kreise_1998_2014[2] <-NULL
names(Kreise_1998_2014)[2] <- "Kreis: Fläche in kmÂ²"
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
Kreise_1998_2014$Kreiskennziffer <- Kreise_1998_2014$Kreiskennziffer/1000

# Create Basedataset
# all levels of input will be added, Kreise is just a starting point.
Basedata    <- as.data.frame(Kreise_1998_2014$Kreiskennziffer) 
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

# Manual separation of data and levels
listofdeterminants <- names(Basedata)[3:length(Basedata)]
Basedata_Gemeindeverbandsebene <- Basedata %>% dplyr::select(Kennziffer,Jahr,Arbeitslosigkeit,Beschäftigtenquote,Einkommenssteuer)
Basedata_Kreisebene <- Basedata %>% dplyr::select(-Arbeitslosigkeit,-Beschäftigtenquote,-Einkommenssteuer)

# load ZIPCode Shapefile
GEM  <- readOGR(dsn = "Data/SHP" ,layer = "VG250_GEM" )
GEM@data$GEN <- iconv(GEM@data$GEN, "UTF-8", "WINDOWS-1252")
GEM@data$BEM <- iconv(GEM@data$BEM, "UTF-8", "WINDOWS-1252")
PLZ5  <- readOGR(dsn = "Data/SHP" ,layer = "PLZ5" )
GEM <- spTransform(GEM, PLZ5@proj4string)

# Intersect ZIP-CODES and 'Gemeinden'
GEM_EWZ <-GEM[as.numeric(as.character(GEM$EWZ))>0,]
GEM_EWZ$Bula <- floor(as.numeric(as.character(GEM_EWZ$AGS))/1000000)
PLZ_to_GEM <- raster::intersect(GEM_EWZ[GEM_EWZ$Bula==1,],PLZ5)

for (i in 2:max(GEM_EWZ$Bula)) {
  cat("\r","ID= ",i," ")
  result <- raster::intersect(GEM_EWZ[GEM_EWZ$Bula==i,],PLZ5)
  PLZ_to_GEM <- rbind(PLZ_to_GEM,result)
  }

PLZ_to_GEM$id <- 1:nrow(PLZ_to_GEM@data)
PLZ_to_GEM$Area_Polygon <- raster::area(PLZ_to_GEM)/1000
PLZ_to_GEM$EWZ <- as.numeric(as.character(PLZ_to_GEM$EWZ))
PLZ_to_GEM@data <- PLZ_to_GEM@data %>% group_by(AGS) %>% mutate(Area_Pct = Area_Polygon/sum(Area_Polygon))
PLZ_to_GEM@data <- PLZ_to_GEM@data %>% group_by(AGS) %>% mutate(EW_Area = round(Area_Pct*EWZ))
PLZ_to_GEM$PLZ4 <- substr(PLZ_to_GEM$PLZ5,1,4)
PLZ_to_GEM$PLZ3 <- substr(PLZ_to_GEM$PLZ5,1,3)
PLZ_to_GEM$PLZ2 <- substr(PLZ_to_GEM$PLZ5,1,2)
PLZ_to_GEM$PLZ1 <- substr(PLZ_to_GEM$PLZ5,1,1)
PLZ.df <- PLZ_to_GEM@data %>% dplyr::select(AGS,EWZ, Area_Polygon, Area_Pct,EW_Area, contains("PLZ"))
names(PLZ.df)[1] <- "Gemeindekennziffer"
names(PLZ.df)[5] <- "Bevölkerung"
PLZ.df$Gemeindekennziffer <-as.numeric(as.character(PLZ.df$Gemeindekennziffer))
PLZ.df <- PLZ.df %>% ungroup()
save(PLZ.df, file="Data/Referenz/Zuspieldatensatz_PLZ_AGS.RData")
head(PLZ.df)
rm(PLZ_to_GEM)

# Join different levels
Basedata <- as.data.frame(expand.grid(Gemeindekennziffer=Referenz_1998_2014$Gemeindekennziffer,Jahr=seq(min(Basedata$Jahr):max(Basedata$Jahr))+min(Basedata$Jahr)-1))
Basedata <- left_join(Basedata,Referenz_1998_2014,by=c("Gemeindekennziffer"))
Basedata$Kreiskennziffer <- as.numeric(Basedata$Kreiskennziffer)/1000
Basedata <- left_join(Basedata,Basedata_Kreisebene,by=c("Kreiskennziffer"="Kennziffer","Jahr"))
Basedata <- left_join(Basedata,Basedata_Gemeindeverbandsebene,by=c("Kennziffer Gemeindeverband"="Kennziffer","Jahr"))
rm(Basedata_Gemeindeverbandsebene,Basedata_Kreisebene,Referenz_1998_2014)

# Change  Storage Types
Basedata[, 13:length(Basedata)] <- sapply(Basedata[, 13:length(Basedata)], as.numeric)

# z-Standardisierung
# Basedata[, 14:length(Basedata)] <- sapply(Basedata[, 14:length(Basedata)], scale)

# Fehlende Werte imputieren
summary(Basedata[, 13:length(Basedata)])[7,] # Anzahl der NA pro Variable

# Imputation
imputationsliste <- subset(listofdeterminants , !(listofdeterminants %in% c('Arbeitslosigkeit','SchulabgaengermitHochschulreife','SchulabgaengerohneAbschluss')))
Impdata <-  Basedata %>% dplyr::filter(Jahr>=1998, Bevölkerung>0)
summary(Impdata)[7,]

for(determinant in imputationsliste) {
  print(paste(determinant))
  Impdata$temp <- Impdata[[determinant]]
  Impdata <- Impdata %>% group_by(Gemeindekennziffer) %>% arrange(Gemeindekennziffer, Jahr) %>% mutate(temp_MEAN = mean(temp, na.rm=T))
  imp.model <- lm(temp ~  I(Jahr*Jahr*temp_MEAN)+I(Jahr*temp_MEAN) + Arbeitslosigkeit + SchulabgaengerohneAbschluss , data = Impdata , na.action="na.exclude")
  summary(imp.model)
  Impdata$temp <- predict(imp.model, newdata =Impdata )
  Impdata[[determinant]][is.na(Impdata[[determinant]])] <-  Impdata$temp[is.na(Impdata[[determinant]])]
  Impdata$temp <- NULL
  Impdata$temp_MEAN <- NULL
}

# Ergebnis
summary(Impdata)
Impdata <- as.data.frame(Impdata)

# Faktorenanalyse
print(listofdeterminants)
TS_Arbeitswelt <- Impdata %>% dplyr::select(Arbeitslosigkeit,Beschäftigtenquote,Bruttoverdienst) 
TS_Einkommen   <- Impdata %>% dplyr::select(Einkommenssteuer,Haushaltseinkommen,Schuldnerquote) 
TS_Bildung     <- Impdata %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 

# PCA rotate
TS_Arbeitswelt.pca <- prcomp(TS_Arbeitswelt, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1)
TS_Arbeitswelt.pca
TS_Einkommen.pca <- prcomp(TS_Einkommen, center = TRUE, scale. = TRUE, retx=TRUE, rank. = 1) 
TS_Einkommen.pca
TS_Bildung.pca <- prcomp(TS_Bildung, center = TRUE, scale. = TRUE, retx=TRUE,rank. = 1) 
TS_Bildung.pca

# Componentoverview
GISD_Komponents <- cbind("Teildimension"="Arbeitswelt","Anteil"=TS_Arbeitswelt.pca$rotation^2,"Score"=TS_Arbeitswelt.pca$rotation) 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Einkommen","Anteil"=TS_Einkommen.pca$rotation^2,"Score"=TS_Einkommen.pca$rotation)) 
GISD_Komponents <- rbind(GISD_Komponents,cbind("Teildimension"="Bildung","Anteil"=TS_Bildung.pca$rotation^2,"Score"=TS_Bildung.pca$rotation)) 
GISD_Komponents <- cbind("Variables"=as.data.frame(rownames(GISD_Komponents)),as.data.frame(GISD_Komponents))
rownames(GISD_Komponents) <- NULL
colnames(GISD_Komponents) <- c("Variable","Dimension","Anteil","Score")
GISD_Komponents$GISD <- "GISD"
GISD_Komponents$Richtung <- ifelse(as.numeric(as.character(GISD_Komponents$Score))<0,"negative","positive")
GISD_Komponents$Anteil <- round(as.numeric(as.character(GISD_Komponents$Anteil))*100,digits=1)
save(GISD_Komponents, file="Data/Other/GISD_Komponents.RData")

# Prediction and normalization
Resultdataset <- Impdata
Resultdataset <- merge(Resultdataset,Kreise_1998_2014,by="Kreiskennziffer")
Resultdataset$TS_Arbeitswelt <- as.numeric(predict(TS_Arbeitswelt.pca, newdata = Impdata))
Resultdataset$TS_Einkommen <- as.numeric(predict(TS_Einkommen.pca , newdata = Impdata))
Resultdataset$TS_Bildung <- as.numeric(predict(TS_Bildung.pca, newdata = Impdata))
summary(Resultdataset %>% dplyr::select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung))
rm(TS_Arbeitswelt,TS_Einkommen,TS_Bildung)
rm(TS_Arbeitswelt.pca,TS_Einkommen.pca,TS_Bildung.pca)

# Correction for Direction of generatet Factors (correlation w. Income should be negative)
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung,use="pairwise.complete.obs")<0) {Resultdataset$TS_Bildung <- Resultdataset$TS_Bildung*-1}
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1}
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1}
Resultdataset %>% dplyr::select(Arbeitslosigkeit,TS_Arbeitswelt,TS_Einkommen,TS_Bildung) %>% cor( use="pairwise.complete.obs")

# Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung <- (Resultdataset$TS_Bildung -min(Resultdataset$TS_Bildung ))/(max(Resultdataset$TS_Bildung )-min(Resultdataset$TS_Bildung ))

# GISD
Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung
Resultdataset$GISD_Score <- (Resultdataset$GISD_Score -min(Resultdataset$GISD_Score ))/(max(Resultdataset$GISD_Score )-min(Resultdataset$GISD_Score ))

# Housekeeping
rm(imp.model,Impdata,Basedata)

# Result
summary(Resultdataset %>% dplyr::select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))
str(Resultdataset %>% dplyr::select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))

# Export by level using for loop
exportlist<- NULL
exportlist$Kennziffern <- c("Gemeindekennziffer","Kreiskennziffer","Kennziffer Gemeindeverband","Raumordnungsregion Nr","NUTS2")
exportlist$Namen <- c("Name der Gemeinde","Name des Kreises","Name Gemeindeverband","Raumordnungsregion","NUTS2 Name")
exportlist$Label <- c("Gemeinde","Kreis","Gemeindeverband","Raumordnungsregion","NUTS2")

for(mykennziffer in exportlist$Kennziffern) {
  myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
  mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung
  outputdata <- Resultdataset 
  
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% dplyr::select(mykennziffer,myname,Bundesland)  
  mergedataset$id <-mergedataset[,1]
  mergedataset <- mergedataset %>% group_by(id) %>% filter(row_number()==1) %>% as.data.frame() %>% dplyr::select(-id)
  
  # Aggregation
  outputdata <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    dplyr::select(Group,Jahr,"Bevölkerung",GISD_Score) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevölkerung), 
              Bevölkerung = sum(Bevölkerung))
  
  # Daten bereinigen
  names(outputdata)[1] <- mykennziffer
  outputdata <- merge(outputdata,mergedataset,by=mykennziffer)
  outputdata <- outputdata %>% dplyr::select(mykennziffer,myname,Jahr,Bundesland,"Bevölkerung",GISD_Score)
  outputdata <- group_by(outputdata,Jahr)
  
  # Rekodierung
  outputdata <- outputdata %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6)) # Normalize to 0/1 
  outputdata <- outputdata %>%  mutate(GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9))) # Set Quantiles
  outputdata <- outputdata %>%  mutate(GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)))
  outputdata <- outputdata %>%  mutate(GISD_10 = findInterval(GISD_10, c(1:10)))
  outputdata <- outputdata %>%  mutate(GISD_5 = findInterval(GISD_5, c(1:5))) # change outliers to adjacent values 
  outputdata <- outputdata %>%  mutate(GISD_k = findInterval(GISD_5, c(1,2,5))) # create Groups
  summary(outputdata)
  
  # Ausgabe Bund
  ListeJahre <- unique(outputdata$Jahr)
  dir.create("Revisions/2018/Bund", showWarnings=F)  
  dir.create( paste0("Revisions/2018/Bund/",mylabel), showWarnings=F)  
  for(myjahr in ListeJahre) {
    mydata <- outputdata %>% filter(Jahr==myjahr) %>% ungroup() %>% dplyr::select(-Jahr,-Bundesland)
    write.csv(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,"_",myjahr,".csv"))
  }
  mydata <- outputdata %>% ungroup() %>% dplyr::select(-Bundesland)
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\ö","oe",names(mydata))
  names(mydata) <- gsub("\\ä","ae",names(mydata))
  names(mydata) <- gsub("\\ü","ue",names(mydata))
  names(mydata) <- gsub("\\ß","ss",names(mydata))
  write_dta(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,"_long.dta"))
  
  # Ausgabe Bundeslandspezifisch ohne Stadtstaaten und nur fÃ¼r Ebenen Kreis und Gemeindeverband
  if (mylabel %in% c("Gemeindeverband","Kreis")) {
  outputdata <- outputdata %>% ungroup() %>% filter(!(Bundesland %in% c("Bremen","Hamburg","Berlin"))) %>% dplyr::select(-GISD_k,-GISD_5,-GISD_10) %>% group_by(Jahr,Bundesland) 
  
  # Rekodierung Bundesland
  outputdata <- outputdata %>%  mutate(GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9))) # Set Quantiles
  outputdata <- outputdata %>%  mutate(GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)))
  outputdata <- outputdata %>%  mutate(GISD_10 = findInterval(GISD_10, c(1:10)))
  outputdata <- outputdata %>%  mutate(GISD_5 = findInterval(GISD_5, c(1:5))) # change outliers to adjacent values 
  outputdata <- outputdata %>%  mutate(GISD_k = findInterval(GISD_5, c(1,2,5))) # create Groups
  summary(outputdata)
  
  # Ausgabe Bundeländer
  ListeBula <- unique(outputdata$Bundesland)
  dir.create("Revisions/2018/Bundesland", showWarnings=F)  
  for(myland in ListeBula) {
  dir.create( paste0("Revisions/2018/Bundesland/",myland), showWarnings=F)  
    dir.create( paste0("Revisions/2018/Bundesland/",myland,"/",mylabel), showWarnings=F)  
    for(myjahr in ListeJahre) {
      mydata <- outputdata %>% filter(Jahr==myjahr,Bundesland==myland) %>% ungroup() %>% dplyr::select(-Jahr,-Bundesland)
      write.csv(mydata, paste0("Revisions/2018/Bundesland/",myland,"/",mylabel,"/",mylabel,"_",myjahr,".csv"))
    }
    mydata <- outputdata %>% filter(Bundesland==myland)
    names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
    names(mydata) <- gsub("\\ö","oe",names(mydata))
    names(mydata) <- gsub("\\ä","ae",names(mydata))
    names(mydata) <- gsub("\\ü","ue",names(mydata))
    names(mydata) <- gsub("\\ß","ss",names(mydata))
    write_dta(mydata, paste0("Revisions/2018/Bundesland/",myland,"/",mylabel,"/",mylabel,"_",myjahr,".dta"))
  }
  }  
}


# Ausgabe auf PLZ-Ebene 
for (mykennziffer in c("PLZ2","PLZ3","PLZ4","PLZ5")) {
  myname <-  paste0(mykennziffer)
  mylabel<-  paste0(mykennziffer)
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung # weighted.mean fehlt wg. Fehler Evaluation error: 'x' and 'w' must have the same length
  outputdata <- Resultdataset 
  outputdata <- outputdata %>% dplyr::select(Gemeindekennziffer,Jahr,GISD_Score)
  outputdata <- left_join(PLZ.df,outputdata,by=c("Gemeindekennziffer"), all.x = TRUE)
  outputdata <- outputdata %>% filter(!is.na(mykennziffer) & !is.na(Bevölkerung) & !is.na(Jahr) & Bevölkerung>0)
  mycol <- which(mykennziffer %in% names(outputdata))
  outputdata <- outputdata %>% group_by(Jahr,Gemeindekennziffer) 
  outputdata <- outputdata %>% mutate(GISD_Score = weighted.mean(GISD_Score,Bevölkerung))
  names(outputdata)[names(outputdata)=="Jahr"]<- "JAHR" # Seltsames Problem Name "Jahr"
  outputdata <- outputdata %>% group_by_at(vars("JAHR",mykennziffer)) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score,Bevölkerung), Bevölkerung = sum(Bevölkerung)) %>%
    group_by(JAHR)
  
  outputdata <- outputdata %>%  mutate(GISD_Score = round((GISD_Score - min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6)) # Normalize to 0/1 
  outputdata <- outputdata %>%  mutate(GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9))) # Set Quantiles
  outputdata <- outputdata %>%  mutate(GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)))
  outputdata <- outputdata %>%  mutate(GISD_10 = findInterval(GISD_10, c(1:10)))
  outputdata <- outputdata %>%  mutate(GISD_5 = findInterval(GISD_5, c(1:5))) # change outliers to adjacent values 
  outputdata <- outputdata %>%  mutate(GISD_k = findInterval(GISD_5, c(1,2,5))) # create Groups
  summary(outputdata)            
  head(outputdata)
  ListeJahre <- unique(outputdata$JAHR)
  dir.create( paste0("Revisions/2018/Bund/",mylabel), showWarnings=F)  
  for(myjahr in ListeJahre) {
    mydata <- outputdata %>% filter(JAHR==myjahr) %>% ungroup() %>% dplyr::select(-JAHR)
    write.csv2(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,"_",myjahr,".csv"))
  }
  mydata <- outputdata %>% ungroup() 
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\ö","oe",names(mydata))
  names(mydata) <- gsub("\\ä","ae",names(mydata))
  names(mydata) <- gsub("\\ü","ue",names(mydata))
  names(mydata) <- gsub("\\ß","ss",names(mydata))
  write_dta(mydata, paste0("Revisions/2018/Bund/",mylabel,"/",mylabel,"_long.dta"))
  }



