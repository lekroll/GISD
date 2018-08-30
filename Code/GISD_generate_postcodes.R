# Get data from OSM - System commands do work only on a Windows Plattform

# Sources for used packages
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_install.R")
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_rki_setup.R")

# Working Directory
mypath <- "C:/Users/krolll/Desktop/GISD"
setwd(mypath)
dir.create("Data/SHP")
setwd("Data/SHP")

## Libraries
library("RCurl") # fetch data
library("rgdal") # load/save shp-files
library("rgeos") # union
library("tidyverse") # data manipulation
library("maptools")  # join Polygons

## Download and Prepare Data and Tools
#=====================================
# Download SHP for Germany on "Gemeinde" level
download.file("http://www.geodatenzentrum.de/auftrag1/archiv/vektor/vg250_ebenen/2014/vg250-ew_2014-12-31.geo89.shape.ebenen.zip","vg250.zip", method="libcurl", mode = "wb")
unzip("vg250.zip", junkpaths = T , files=c("vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.cpg",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.dbf",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.prj",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shp",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shx"))
file.remove("vg250.zip")

## Download Planet File for Germany
### This step may be problematic, depending on server load
download.file("http://ftp5.gwdg.de/pub/misc/openstreetmap/download.geofabrik.de/germany-latest.osm.pbf",
               method="libcurl",destfile="germany-latest.osm.pbf", mode = "wb")

## Download binary OSM-Tools for Windows
download.file("http://m.m.i24.cc/osmconvert64.exe", destfile = "osmconvert64.exe" ,  method="libcurl", mode = "wb")
download.file("http://m.m.i24.cc/osmfilter.exe", destfile = "osmfilter.exe" , method="libcurl", mode = "wb")

# Convert Planet-File to filter-friendly o5m-format
system("osmconvert64.exe germany-latest.osm.pbf  --drop-author --drop-version --out-o5m -o=germany-latest.o5m")
file.remove('germany-latest.osm.pbf')

# Extract Features from OSM
#==========================
# ZIP-Codes
system2("osmfilter.exe",args = c('germany-latest.o5m' ,'--keep=\"boundary=postal_code\"', '--drop-author', '--drop-version', '--out-osm'), stdout='Germany_zipcodes.osm')
system2("osmconvert64.exe" , args=c("Germany_zipcodes.osm", "--drop-author" ,"--drop-version" ,"-o=Germany_zipcodes.osm.pbf"))
file.remove("Germany_zipcodes.osm")
file.remove("germany-latest.o5m")

### ZIP-Codes 5 digits
zipcodes <- readOGR("Germany_zipcodes.osm.pbf", 'multipolygons')
file.remove("Germany_zipcodes.osm.pbf")
zipcodes <- zipcodes[!is.na(zipcodes$osm_id),] 
zipcodes@data$PLZ5 <- str_split(zipcodes@data$other_tags, ",",simplify = T)
zipcodes@data$PLZ5 <- apply(zipcodes@data$PLZ5, 1, function(x) unlist(regmatches(x, gregexpr("^\"postal_code\".*",  x))))
zipcodes@data$PLZ5 <- apply(as.data.frame(zipcodes@data$PLZ5), 1, function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+",  x))))
zipcodes@data <- zipcodes@data %>% select(PLZ5) 
writeOGR(zipcodes, ".", "PLZ5", driver="ESRI Shapefile",overwrite_layer=TRUE)

# HIERWEITER!

### ZIP-Codes 4 digits
zipcodes@data$PLZ4 <- floor(zipcodes@data$PLZ5/10)
PLZ4 <- gUnaryUnion(zipcodes, id=zipcodes@data$PLZ4)
rm(zipcodes)
zipcode_ids <- data.frame(ID=sapply(slot(PLZ4, "polygons"), function(x) slot(x, "ID")))
zipcode_df <- over(PLZ4,zipcodes)
rownames(zipcode_df) <- zipcode_ids$ID
PLZ4 <- SpatialPolygonsDataFrame(PLZ4,zipcode_df)
PLZ4@data <- PLZ4@data %>% select(PLZ4) 
writeOGR(PLZ4, ".", "PLZ4", driver="ESRI Shapefile",overwrite_layer=TRUE)

### ZIP-Codes 3 digits
PLZ4@data$PLZ3 <- floor(PLZ4@data$PLZ4/10)
PLZ3 <- gUnaryUnion(PLZ4, id=PLZ4@data$PLZ3)
zipcode_ids <- data.frame(ID=sapply(slot(PLZ3, "polygons"), function(x) slot(x, "ID")))
zipcode_df <- over(PLZ3,PLZ4)
rownames(zipcode_df) <- zipcode_ids$ID
PLZ3 <- SpatialPolygonsDataFrame(PLZ3,zipcode_df)
PLZ3@data <- PLZ3@data %>% select(PLZ3) 
writeOGR(PLZ3, ".", "PLZ3", driver="ESRI Shapefile",overwrite_layer=TRUE)

### ZIP-Codes 2 digits
zipcodes@data$PLZ2 <- floor(zipcodes@data$PLZ5/1000)
PLZ2 <- gUnaryUnion(zipcodes, id=zipcodes@data$PLZ2)
zipcode_ids <- data.frame(ID=sapply(slot(PLZ2, "polygons"), function(x) slot(x, "ID")))
zipcode_df <- over(PLZ2,zipcodes)
rownames(zipcode_df) <- zipcode_ids$ID
PLZ2 <- SpatialPolygonsDataFrame(PLZ2,zipcode_df)
PLZ2@data <- PLZ2@data %>% select(PLZ2) 
writeOGR(PLZ2, ".", "PLZ2", driver="ESRI Shapefile",overwrite_layer=TRUE)

### ZIP-Codes 1 digit
zipcodes@data$PLZ1 <- floor(zipcodes@data$PLZ5/10000)
PLZ1 <- gUnaryUnion(zipcodes, id=zipcodes@data$PLZ1)
zipcode_ids <- data.frame(ID=sapply(slot(PLZ1, "polygons"), function(x) slot(x, "ID")))
zipcode_df <- over(PLZ1,zipcodes)
rownames(zipcode_df) <- zipcode_ids$ID
PLZ1 <- SpatialPolygonsDataFrame(PLZ1,zipcode_df)
rm(zipcode_ids,zipcode_df)
PLZ1@data <- PLZ1@data %>% select(PLZ1) 
writeOGR(PLZ1, ".", "PLZ1", driver="ESRI Shapefile",overwrite_layer=TRUE)