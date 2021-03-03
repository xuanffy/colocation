# This part is from Lidsay's GitHub code file
library(sf)
library(tidyverse)
pb <- st_read("data/Prison_Boundaries.shp", stringsAsFactors = FALSE)
pb_sf <- st_transform(pb, crs = 4269)
#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269
#longitude and latitude information are stored in "geometry" feature
pb_loc<-pb_sf%>% select(FID,geometry)


sfs <- read.csv("data/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LONGITUDE83))
pb_crs <- st_crs(pb_sf)
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)
sfs_sf <- sfs_sf %>% rename(ID = X)
#longitude and latitude information are stored in "geometry" feature
sfs_loc<-sfs_sf%>% select(ID,geometry)

###The location info are saved in local.rda file
save(pb_loc,sfs_loc,file = "data/local.rda")
rm(list=ls())
load("data/local.rda")
