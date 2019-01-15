# Get data from OSM - System commands do work only on a Windows Plattform

# Sources for used packages
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_install.R")
# source("http://raw.githubusercontent.com/lekroll/R/master/files/r_rki_setup.R")

# Working Directory
mypath <- "P:/Daten/github/GISD"
setwd(mypath)

## Libraries
library("RCurl") # fetch data
library("tidyverse") # data manipulation
library("sf") # simple features fp
library("rmapshaper") # simplify shapefiles

## Download and Prepare Data and Tools
#=====================================
tempdata <- tempdir()

# Download SHP for Germany on "Gemeinde" level
#===============================================
download.file("http://www.geodatenzentrum.de/auftrag1/archiv/vektor/vg250_ebenen/2014/vg250-ew_2014-12-31.geo89.shape.ebenen.zip",paste0(tempdata,"vg250.zip"), method="libcurl", mode = "wb")
unzip(paste0(tempdata,"vg250.zip"), exdir=tempdata, junkpaths = T , files=c("vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.cpg",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.dbf",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.prj",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shp",
                                                      "vg250-ew_2014-12-31.geo89.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shx"))
GEM  <- st_read(paste0(tempdata,"/VG250_GEM.shp"))
file.remove(paste0(tempdata,c("/VG250_GEM.shp","/VG250_GEM.dbf","/VG250_GEM.prj","/VG250_GEM.shx","/VG250_GEM.cpg")), showWarnings=F)

# Download OSM Planet File for Germany
#=====================================
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
# # Convert OSM and read as sf dataset
# system2(paste0(tempdata,"/osmfilter.exe"),args = c(paste0(tempdata,'/germany-latest.o5m') ,'--keep=\"boundary=postal_code\"', '--drop-author', '--drop-version', '--out-osm'), stdout=paste0(tempdata,'/Germany_zipcodes.osm'))
# system2(paste0(tempdata,"/osmconvert64.exe") , args=c(paste0(tempdata,"/Germany_zipcodes.osm"), "--drop-author" ,"--drop-version" ,paste0("-o=",tempdata,"/Germany_zipcodes.osm.pbf")))
# file.remove(paste0(tempdata,"/Germany_zipcodes.osm"))
# file.remove(paste0(tempdata,"/germany-latest.o5m"))
# zipcodes <- st_read(paste0(tempdata,"/Germany_zipcodes.osm.pbf"), layer='multipolygons')
# file.remove(paste0(tempdata,"/osmconvert64.exe"))
# file.remove(paste0(tempdata,"/osmfilter.exe"))
# 
# ### ZIP-Codes 1-5 digits
# zipcodes <- zipcodes %>% filter(!is.na(osm_id))
# zipcodes$PLZ5 <- str_split(zipcodes$other_tags, ",",simplify = T)
# zipcodes$PLZ5 <- apply(zipcodes$PLZ5, 1, function(x) unlist(regmatches(x, gregexpr("^\"postal_code\".*",  x))))
# zipcodes$PLZ5 <- apply(as.data.frame(zipcodes$PLZ5), 1, function(x) as.numeric(regmatches(x, gregexpr("[[:digit:]]+",  x))))
# zipcodes  <- zipcodes %>% select(PLZ5) %>%
#   mutate(PLZ4= floor(PLZ5/10),
#          PLZ3= floor(PLZ4/10),
#          PLZ2= floor(PLZ3/10),
#          PLZ1= floor(PLZ2/10))
# zicodes_small <- ms_simplify(zipcodes, keep=.1 , keep_shapes=T)
# zipcodes <- zicodes_small
# rm(zicodes_small)
# save("zipcodes", file="Data/SHP/zipcodes.RData", compression_level=9)

# Combine Zipcodes and Areas
#===========================
load("Data/SHP/zipcodes.RData")

# Recode
GEM <- GEM %>% mutate(GEM_EWZ=as.numeric(as.character(EWZ))>0,
               Bula= floor(as.numeric(as.character(AGS))/1000000))
# Set CRS
st_crs(zipcodes)
st_crs(GEM)
zipcodes <- st_transform(zipcodes,st_crs(GEM))
st_crs(zipcodes)

# Intersect Communities with Postcodes
PLZ.df <- st_intersection(GEM,zipcodes)

# Fill Intersection Area equally with all inhabitants by community
PLZ.df <- PLZ.df %>% mutate(Area_Polygon=as.numeric(st_area(.))) %>% 
              group_by(AGS) %>% 
              mutate(Area_Pct = Area_Polygon/sum(Area_Polygon),
                     EW_Area  = round(Area_Pct*EWZ)) %>% 
              dplyr::select(AGS,GEN,EWZ, Area_Polygon, Area_Pct,EW_Area, contains("PLZ")) %>%  st_set_geometry(NULL)

save(PLZ.df, file="Data/SHP/GEM_Zipcode_Intersections.RData", compression_level=9)


