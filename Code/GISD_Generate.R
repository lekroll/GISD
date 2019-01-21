# GISD - German Index of Socio-Economic Deprivation
# Author: Lars Eric Kroll, Robert Koch Institut, Berlin
# Citation: Kroll LE, Schumann M, Hoebel J et al. (2017) Regional health differences - developing a socioeconomic deprivation 
#           index for Germany. Journal of Health Monitoring 2(2):98-114. DOI 10.17886/RKI-GBE-2017-048ISSN 2511-2708

# Revision: 2018.v3
# Date: 2018-09-04

# SOP for Revision
# 1. Obtain new Data and Reference Files from INKAR (manually) -> check format
# 2. Change Year in GISD_generate_postcodes.R according to INKAR Regional Date and execute
# 3. Execute GISD_Generate.R (there should be no edits required)

# Librarys
require("tidyverse") # Tidyverse Methods
require("readxl") # Read Excel
require("imputeTS") # Impute Missing Features
require("haven") # write Stata-dta
require("sf") # write Stata-dta


# Create Output directories in working directoryif necessary
dir.create("Revisions/2019")
dir.create("Revisions/2019/Bund")
dir.create("Revisions/2019/Other")

# Import data
Gemeinden_INKAR <- read_excel("Data/Referenz/Referenz_1998_2015.xlsx", sheet = "Gemeinden", na = "NA", skip = 2) %>% 
  rename(Kennziffer=gem15,"Kennziffer Gemeindeverband"="Gemeindeverband, Stand 31.12.2015") %>% filter(!is.na(Kennziffer))

Gemeindeverbände_INKAR <- read_excel("Data/Referenz/Referenz_1998_2015.xlsx", sheet = "GVB 2015", na = "NA", skip = 2) %>% 
  select("Kennziffer Gemeindeverband"=gvb15,"Name des Gemeindeverbands") %>% filter(!is.na("Kennziffer Gemeindeverband")) 
  

Kreise_INKAR <- read_excel("Data/Referenz/Referenz_1998_2015.xlsx", sheet = "Kreise", skip = 2) %>%
 mutate(Kennziffer = as.numeric(krs15)/1000) %>% filter(!is.na(Kennziffer))

# ID-Dataset
id_dataset <- Gemeinden_INKAR %>% 
              select(Gemeindekennziffer=Kennziffer,"Name der Gemeinde"=`Gemeindename 2015`,"Kennziffer Gemeindeverband") %>% 
              mutate(Kreiskennziffer=floor(Gemeindekennziffer/1000)) %>%
              left_join(.,Kreise_INKAR %>% select(Kreiskennziffer=krs15,
                                                  "Name des Kreises"=krs15name,
                                                  "Raumordnungsregion Nr"=ROR11,
                                                  Raumordnungsregion=ROR11name,
                                                  NUTS2,
                                                  "NUTS2 Name"=NUTS2name,
                                                  Bundesland=X__5) %>% 
                          mutate(Kreiskennziffer=floor(Kreiskennziffer/1000)),
                        by="Kreiskennziffer") %>%
              left_join(.,Gemeindeverbände_INKAR, by="Kennziffer Gemeindeverband")

# Create Basedataset
# all levels of input will be added, Kreise is just a starting point.
Basedata    <- Kreise_INKAR %>% select(Kennziffer) %>% mutate(Jahr=2014)

# Load INKAR datasets
inputdataset <- list.files("Data/INKAR_1998_2015")

# for testing file<-inputdataset[1]
for(file in inputdataset){
  myimport <- read_excel(paste0("Data/INKAR_1998_2015/",file), skip = 1, sheet = "Daten", col_types = c("text"))
  names(myimport)[1] <- "Kennziffer"
  myimport[3] <- NULL
  myimport[2] <- NULL
  myimport <- myimport %>% gather(key = "Jahr", value = "Value" , -"Kennziffer", convert=T, na.rm = T) %>%
    mutate(Kennziffer=as.numeric(as.character(Kennziffer)), Value=as.numeric(Value)) 
  names(myimport)[3] <- unlist(strsplit(unlist(strsplit(file,"_"))[2],"[.]"))[1]
  Basedata <- full_join(Basedata,myimport,by=c("Kennziffer","Jahr"))
  }
rm(inputdataset) 
names(Basedata)

# Manual separation of data and levels
listofdeterminants <- names(Basedata)[3:length(Basedata)]
Basedata_Gemeindeverbandsebene <- Basedata %>% dplyr::select(Kennziffer,Jahr,Arbeitslosigkeit,Beschaeftigtenquote,Einkommenssteuer) %>%   
  gather(key,value,3:5) %>% filter(!is.na(value)) %>% spread(key,value) %>% filter(Jahr>=1998) %>% rename("Gemeindeverband"=Kennziffer)
Basedata_Kreisebene <- Basedata %>% select(krs15=Kennziffer,Jahr,listofdeterminants) %>% 
  select(-Arbeitslosigkeit,-Einkommenssteuer,-Beschaeftigtenquote) %>% rename(Kreis=krs15)
  
# Join different levels
Workfile <- as.data.frame(expand.grid("Kennziffer"=Gemeinden_INKAR %>% pull(Kennziffer),"Jahr"=seq(min(Basedata$Jahr):max(Basedata$Jahr))+min(Basedata$Jahr)-1)) %>%
   mutate(Kreiskennziffer=floor(as.numeric(Kennziffer)/1000)) %>% as.tibble() %>%
   left_join(. , Gemeinden_INKAR,by=c("Kennziffer"))  %>%
   select(Gemeindekennziffer=Kennziffer,Kreis=Kreiskennziffer,Gemeindeverband="Kennziffer Gemeindeverband",Jahr,Bevölkerung=`Bevölkerung 31.12.2015`) %>% 
      arrange(Gemeindekennziffer,Jahr) %>% # Join Metadata
   left_join(. , Basedata_Kreisebene,by=c("Kreis","Jahr")) %>% # Join Indicators for Level: Kreis
   left_join(. , Basedata_Gemeindeverbandsebene,by=c("Gemeindeverband","Jahr")) %>%  # Join Indicators for Level: Gemeindeverband 
   filter(Jahr>=1998)
names(Workfile)

# Impute Missing Values
summary(Workfile %>% select(listofdeterminants))

# Imputation
imputationsliste <- subset(listofdeterminants , 
                           !(listofdeterminants %in% 
                               c('Arbeitslosigkeit','SchulabgaengermitHochschulreife','SchulabgaengerohneAbschluss')))
Impdata <-  Workfile %>%  dplyr::filter(Jahr>=1998, Bevölkerung>0) %>% 
  gather(key,value,6:15) %>% mutate(value=ifelse(value<0,NA,value)) %>% spread(key,value)
summary(Impdata %>% select(listofdeterminants))
names(Impdata)

# Impute_function (NOT FOR GROUPED DATA!)
my_ts_imputer <- function(data,outcome_name){
  mydata   <- data %>% group_by(Gemeindekennziffer) %>% select(Gemeindekennziffer,Jahr,Arbeitslosigkeit,SchulabgaengerohneAbschluss,SchulabgaengermitHochschulreife,"Outcome"=paste(outcome_name)) %>% 
    mutate(MEAN=mean(Outcome , na.rm=T)) %>% ungroup()
  mymodell <- lm(Outcome ~
                   I(Jahr*Jahr*MEAN)+I(Jahr*MEAN) + Arbeitslosigkeit + 
                   SchulabgaengerohneAbschluss ,
                   data = mydata  , na.action="na.exclude")
  mydata %>% select(Outcome) %>% mutate(Imputed = predict(mymodell, newdata =mydata )) %>%
    mutate(Outcome=ifelse(is.na(Outcome),Imputed,Outcome)) %>% 
    mutate(Outcome=ifelse(Outcome<0,0,Outcome)) %>% pull(Outcome)
  }

# Test Funtion if necessary
# Impdata %>% mutate(Test=my_ts_imputer(.,"Bruttoverdienst")) %>% select(Gemeindekennziffer,Jahr,Bruttoverdienst,Test) %>% head()

Impdata.imputed <- Impdata %>% mutate(
  Beschaeftigtenquote=my_ts_imputer(.,"Beschaeftigtenquote"),
  Bruttoverdienst=my_ts_imputer(.,"Bruttoverdienst"),
  BeschaeftigtemitakadAbschluss=my_ts_imputer(.,"BeschaeftigtemitakadAbschluss"),
  BeschaeftigteohneAbschluss=my_ts_imputer(.,"BeschaeftigteohneAbschluss"),
  Einkommenssteuer=my_ts_imputer(.,"Einkommenssteuer"),
  Haushaltseinkommen=my_ts_imputer(.,"Haushaltseinkommen"),
  Schuldnerquote=my_ts_imputer(.,"Schuldnerquote")           
  )


# Result of Imputation
summary(as.data.frame(Impdata.imputed) %>% ungroup()  %>% select(listofdeterminants))

# Faktorenanalyse
print(listofdeterminants)
TS_Arbeitswelt <- Impdata.imputed %>% dplyr::select(Beschaeftigtenquote,Arbeitslosigkeit,Bruttoverdienst) 
TS_Einkommen   <- Impdata.imputed %>% dplyr::select(Einkommenssteuer,Haushaltseinkommen,Schuldnerquote) 
TS_Bildung     <- Impdata.imputed %>% dplyr::select(BeschaeftigtemitakadAbschluss,BeschaeftigteohneAbschluss,SchulabgaengerohneAbschluss) 

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
GISD_Komponents$Proportion <- round(as.numeric(as.character(GISD_Komponents$Anteil))*100,digits=1)

# Prediction and normalization
Resultdataset <- Impdata.imputed
Resultdataset$TS_Arbeitswelt <- as.numeric(predict(TS_Arbeitswelt.pca, newdata = Impdata.imputed))
Resultdataset$TS_Einkommen <- as.numeric(predict(TS_Einkommen.pca , newdata = Impdata.imputed))
Resultdataset$TS_Bildung <- as.numeric(predict(TS_Bildung.pca, newdata = Impdata.imputed))
summary(Resultdataset %>% dplyr::select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung))

# Correction for Direction of generatet Factors (correlation w. Income should be negative)
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Bildung,use="pairwise.complete.obs")<0) {
   Resultdataset$TS_Bildung <- Resultdataset$TS_Bildung*-1
   }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Arbeitswelt,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Arbeitswelt <- Resultdataset$TS_Arbeitswelt*-1
  }
if (cor(Resultdataset$Arbeitslosigkeit, Resultdataset$TS_Einkommen,use="pairwise.complete.obs")<0) {
  Resultdataset$TS_Einkommen <- Resultdataset$TS_Einkommen*-1
}

Resultdataset %>% dplyr::select(Arbeitslosigkeit,TS_Arbeitswelt,TS_Einkommen,TS_Bildung) %>% cor( use="pairwise.complete.obs")
GISD_Komponents
save(GISD_Komponents, file="Revisions/2019/Other/GISD_Komponents.RData")


# Normalization
Resultdataset$TS_Arbeitswelt <- (Resultdataset$TS_Arbeitswelt -min(Resultdataset$TS_Arbeitswelt ))/(max(Resultdataset$TS_Arbeitswelt )-min(Resultdataset$TS_Arbeitswelt ))
Resultdataset$TS_Einkommen <- (Resultdataset$TS_Einkommen -min(Resultdataset$TS_Einkommen ))/(max(Resultdataset$TS_Einkommen )-min(Resultdataset$TS_Einkommen ))
Resultdataset$TS_Bildung <- (Resultdataset$TS_Bildung -min(Resultdataset$TS_Bildung ))/(max(Resultdataset$TS_Bildung )-min(Resultdataset$TS_Bildung ))

# GISD
Resultdataset$GISD_Score <- Resultdataset$TS_Arbeitswelt+Resultdataset$TS_Einkommen+Resultdataset$TS_Bildung
Resultdataset$GISD_Score <- (Resultdataset$GISD_Score -min(Resultdataset$GISD_Score ))/(max(Resultdataset$GISD_Score )-min(Resultdataset$GISD_Score ))

# Result
summary(Resultdataset %>% select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))
str(Resultdataset %>% select(TS_Arbeitswelt,TS_Einkommen,TS_Bildung,GISD_Score))

Resultdataset <- Resultdataset %>% select(Gemeindekennziffer,Jahr,Bevölkerung,contains("TS_"),contains("GISD_Score"))

# Merge IDs to Resultdataset
RawResult <- left_join(Resultdataset,id_dataset,by="Gemeindekennziffer")


# Export by level using for loop
exportlist<- NULL
exportlist$Kennziffern <- c("Gemeindekennziffer","Kreiskennziffer","Kennziffer Gemeindeverband","Raumordnungsregion Nr","NUTS2")
exportlist$Namen <- c("Name der Gemeinde","Name des Kreises","Name des Gemeindeverbands","Raumordnungsregion","NUTS2 Name")
exportlist$Label <- c("Gemeinde","Kreis","Gemeindeverband","Raumordnungsregion","NUTS2")
mykennziffer <-"Gemeindekennziffer" # for testing


for(mykennziffer in exportlist$Kennziffern) {
  myname <-  exportlist$Namen[exportlist$Kennziffern==mykennziffer]
  mylabel<-  exportlist$Label[exportlist$Kennziffern==mykennziffer]
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung
  outputdata <- RawResult 
  
  outputdata$Group <- outputdata[[mykennziffer]]
  mergedataset  <- outputdata %>% dplyr::select(ID=mykennziffer,myname,Bundesland) %>% 
    group_by(ID) %>% filter(row_number()==1) %>% ungroup() 
  names(mergedataset)[1]=mykennziffer
  
  # Aggregation
  outputdata.agg <- outputdata %>% 
    group_by(Group,Jahr) %>% 
    dplyr::select(Group,Jahr,"Bevölkerung",GISD_Score) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score, Bevölkerung), 
              Bevölkerung = sum(Bevölkerung))
  
  # Daten bereinigen
  names(outputdata.agg)[1] <- mykennziffer
  outputdata.agg <- merge(outputdata.agg,mergedataset,by=mykennziffer) %>%  
    dplyr::select(mykennziffer,myname,Jahr,Bundesland,"Bevölkerung",GISD_Score) %>%
    group_by(Jahr) %>% as.tibble()
  
  # Rekodierung
  outputdata.agg <- outputdata.agg %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                       GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                       GISD_5 = findInterval(GISD_5, c(1:5)),
                                       GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                       GISD_10 = findInterval(GISD_10, c(1:10)),
                                       GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata.agg %>% select(contains("GISD")))
  
  # Ausgabe Bund
  dir.create("Revisions/2019/Bund", showWarnings=F)  
  dir.create( paste0("Revisions/2019/Bund/",mylabel), showWarnings=F)  
  mydata <- outputdata.agg %>% ungroup() %>% dplyr::select(-Bundesland)
  write.csv(mydata, paste0("Revisions/2019/Bund/",mylabel,"/",mylabel,".csv"))
  
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\ö","oe",names(mydata))
  names(mydata) <- gsub("\\ä","ae",names(mydata))
  names(mydata) <- gsub("\\ü","ue",names(mydata))
  names(mydata) <- gsub("\\ß","ss",names(mydata))
  write_dta(mydata, paste0("Revisions/2019/Bund/",mylabel,"/",mylabel,"_long.dta"))
  
  # Ausgabe Bundeslandspezifisch ohne Stadtstaaten und nur fÃ¼r Ebenen Kreis und Gemeindeverband
  if (mylabel %in% c("Gemeindeverband","Kreis")) {
  outputdata.agg <- outputdata.agg %>% ungroup() %>% filter(!(Bundesland %in% c("Bremen","Hamburg","Berlin"))) %>% dplyr::select(-GISD_k,-GISD_5,-GISD_10) %>% group_by(Jahr,Bundesland) 
  
  # Rekodierung Bundesland
  outputdata.agg <- outputdata.agg %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                               GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                               GISD_5 = findInterval(GISD_5, c(1:5)),
                                               GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                               GISD_10 = findInterval(GISD_10, c(1:10)),
                                               GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata)
  
  # Ausgabe Bundeländer
  ListeBula <- unique(outputdata$Bundesland)
  dir.create("Revisions/2019/Bundesland", showWarnings=F)  
  for(myland in ListeBula) {
  dir.create( paste0("Revisions/2019/Bundesland/",myland), showWarnings=F)  
    dir.create( paste0("Revisions/2019/Bundesland/",myland,"/",mylabel), showWarnings=F)  
    mydata <- outputdata %>% filter(Bundesland==myland) %>% ungroup() %>% dplyr::select(-Bundesland)
    write.csv(mydata, paste0("Revisions/2019/Bundesland/",myland,"/",mylabel,"/",mylabel,".csv"))
    
    mydata <- outputdata %>% filter(Bundesland==myland)
    names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
    names(mydata) <- gsub("\\ö","oe",names(mydata))
    names(mydata) <- gsub("\\ä","ae",names(mydata))
    names(mydata) <- gsub("\\ü","ue",names(mydata))
    names(mydata) <- gsub("\\ß","ss",names(mydata))
    write_dta(mydata, paste0("Revisions/2019/Bundesland/",myland,"/",mylabel,"/",mylabel,".dta"))
  }
  }  
}


# Output Postcode Data
load("Data/SHP/GEM_Zipcode_Intersections_2015.RData") # AGS/Postcode-Intersections-Dataset in sf format


for (mykennziffer in c("PLZ2","PLZ3","PLZ4","PLZ5")) {
  myname <-  paste0(mykennziffer)
  mylabel<-  paste0(mykennziffer)
  print(paste("Level:",myname,"Label:",mylabel))
  
  # Datensatzerstellung # weighted.mean fehlt wg. Fehler Evaluation error: 'x' and 'w' must have the same length
  outputdata <- Resultdataset 
  outputdata <- outputdata %>% dplyr::select(AGS=Gemeindekennziffer,Jahr,GISD_Score)
  outputdata <- left_join(as.data.frame(PLZ.df) %>% ungroup() %>% mutate(AGS=as.numeric(as.character(AGS))),
                          outputdata,by=c("AGS"), all.x = TRUE)
  outputdata <- outputdata %>% filter(!is.na(mykennziffer) & !is.na(EW_Area) & !is.na(Jahr) & EW_Area>0)
  mycol <- which(mykennziffer %in% names(outputdata))
  outputdata <- outputdata %>% group_by(Jahr,AGS) 
  outputdata <- outputdata %>% mutate(GISD_Score = weighted.mean(GISD_Score,EW_Area))
  names(outputdata)[names(outputdata)=="Jahr"]<- "JAHR" # Seltsames Problem Name "Jahr"
  outputdata <- outputdata %>% group_by_at(vars("JAHR",mykennziffer)) %>% 
    summarise(GISD_Score = weighted.mean(GISD_Score,EW_Area), Bevölkerung = sum(EW_Area)) %>%
    group_by(JAHR)
  
  outputdata <- outputdata %>%  mutate(GISD_Score = round((GISD_Score -min(GISD_Score ))/(max(GISD_Score )-min(GISD_Score )), digits=6),
                                       GISD_5 = findInterval(GISD_Score, quantile(GISD_Score,   probs=0:5/5 , type=9)),
                                       GISD_5 = findInterval(GISD_5, c(1:5)),
                                       GISD_10 = findInterval(GISD_Score, quantile(GISD_Score, probs=0:10/10 , type=9)),
                                       GISD_10 = findInterval(GISD_10, c(1:10)),
                                       GISD_k = findInterval(GISD_5, c(1,2,5))) 
  summary(outputdata)            
  head(outputdata)
  ListeJahre <- unique(outputdata$JAHR)
  dir.create( paste0("Revisions/2019/Bund/",mylabel), showWarnings=F)  
  mydata <- outputdata %>% ungroup() 
  write.csv2(mydata, paste0("Revisions/2019/Bund/",mylabel,"/",mylabel,".csv"))
  mydata <- outputdata %>% ungroup() 
  names(mydata) <- gsub("\\.","_",make.names(names(mydata)))
  names(mydata) <- gsub("\\ö","oe",names(mydata))
  names(mydata) <- gsub("\\ä","ae",names(mydata))
  names(mydata) <- gsub("\\ü","ue",names(mydata))
  names(mydata) <- gsub("\\ß","ss",names(mydata))
  write_dta(mydata, paste0("Revisions/2019/Bund/",mylabel,"/",mylabel,"_long.dta"))
  }



