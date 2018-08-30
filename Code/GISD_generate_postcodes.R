# Get data from OSM - System commands do work only on a Windows Plattform

# Sources for used packages
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_install.R")
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_rki_setup.R")

# Working Directory
mypath <- "P:/Daten/github/GISD"
setwd(mypath)

tempdata <- tempdir()

## Libraries
library("RCurl") # fetch data
library("tidyverse") # data manipulation
library("sf") # simple features fp

## Download and Prepare Data and Tools
#=====================================
# Download SHP for Germany on "Gemeinde" level
download.file("http://www.geodatenzentrum.de/auftrag1/archiv/vektor/vg250_ebenen/2014/vg250-ew_2014-12-31.geo89.shape.ebenen.zip",paste0(tempdata,"vg250.zip"), method="libcurl", mode = "wb")
unzip(paste0(tempdata,"vg250.zip"), exdir=tempdata, junkpaths = T , files=c("vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.cpg",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.dbf",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.prj",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shp",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shx"))
GEM  <- st_read(paste0(tempdata,"/VG250_GEM.shp"))
file.remove(paste0(tempdata,c("/VG250_GEM.shp","/VG250_GEM.dbf","/VG250_GEM.prj","/VG250_GEM.shx","/VG250_GEM.cpg")), showWarnings=F)
save("GEM", file="Data/SHP/GEM20141231.RData", compression_level=9)


# Download Planet File for Germany
# This step may take a while (up to 1h), depending on server load, but in the end, the huge approx. 3 GB file will be loaded
download.file("http://ftp5.gwdg.de/pub/misc/openstreetmap/download.geofabrik.de/germany-latest.osm.pbf",
               method="libcurl",cacheOK = FALSE, destfile=paste0(tempdata,"/germany-latest.osm.pbf"), mode = "wb")

# Download binary OSM-Tools for Windows
download.file("http://m.m.i24.cc/osmconvert64.exe", destfile = paste0(tempdata,"/osmconvert64.exe") ,  method="libcurl", mode = "wb")
download.file("http://m.m.i24.cc/osmfilter.exe", destfile = paste0(tempdata,"/osmfilter.exe") , method="libcurl", mode = "wb")

# Convert Planet-File to filter-friendly o5m-format
system(paste0(tempdata,"/osmconvert64.exe ", tempdata,"/germany-latest.osm.pbf  --drop-author --drop-version --out-o5m -o=", tempdata,"/germany-latest.o5m"))
file.remove(paste0(tempdata,'/germany-latest.osm.pbf'))

# Extract Features from OSM
#==========================
# Convert OSM and read as sf dataset
system2(paste0(tempdata,"/osmfilter.exe"),args = c(paste0(tempdata,'/germany-latest.o5m') ,'--keep=\"boundary=postal_code\"', '--drop-author', '--drop-version', '--out-osm'), stdout=paste0(tempdata,'/Germany_zipcodes.osm'))
system2(paste0(tempdata,"/osmconvert64.exe") , args=c(paste0(tempdata,"/Germany_zipcodes.osm"), "--drop-author" ,"--drop-version" ,paste0("-o=",tempdata,"/Germany_zipcodes.osm.pbf")))
file.remove(paste0(tempdata,"/Germany_zipcodes.osm"))
file.remove(paste0(tempdata,"/germany-latest.o5m"))
zipcodes <- st_read(paste0(tempdata,"/Germany_zipcodes.osm.pbf"), layer='multipolygons')
file.remove(paste0(tempdata,"/osmconvert64.exe"))
file.remove(paste0(tempdata,"/osmfilter.exe"))

### ZIP-Codes 1-5 digits
zipcodes <- zipcodes %>% filter(!is.na(osm_id))
zipcodes$PLZ5 <- str_split(zipcodes$other_tags, ",",simplify = T)
zipcodes$PLZ5 <- apply(zipcodes$PLZ5, 1, function(x) unlist(regmatches(x, gregexpr("^\"postal_code\".*",  x))))
zipcodes$PLZ5 <- apply(as.data.frame(zipcodes$PLZ5), 1, function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+",  x))))
zipcodes  <- zipcodes %>% select(PLZ5) %>%
  mutate(PLZ4= floor(PLZ5/10),
         PLZ3= floor(PLZ4/10),
         PLZ2= floor(PLZ3/10),
         PLZ1= floor(PLZ2/10))
save("zipcodes", file="Data/SHP/zipcodes.RData", compression_level=9)

# Combine Zipcodes and Areas
#===========================

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
