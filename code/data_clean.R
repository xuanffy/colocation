library(sf)
library(tidyverse)

#### Data Fetching####
# This part is from Lidsay's GitHub code file
pb <- st_read("data/Prison_Boundaries.shp", stringsAsFactors = FALSE)
pb_sf <- st_transform(pb, crs = 4269)
#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269
pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids


#Read airports data file and convert to sf
ap <- read.csv("data/airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read military bases data file
mil <- st_read("data/military_bases.csv", stringsAsFactors = FALSE) 
mil<-mil%>%filter(!is.na(as.numeric(X)))
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read superfund sites data file and convert to sf
sfs <- read.csv("data/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LONGITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Read TRI sites data file and convert to sf
tri <- read.csv("data/tri.csv", stringsAsFactors = FALSE) 
tri <- tri %>% filter(!is.na(X12..LATITUDE))
tri_sf <- st_as_sf(tri, coords = c("X13..LONGITUDE", "X12..LATITUDE"), crs = pb_crs, na.fail = FALSE)

#Rename ID columns so they all match
pb_sf<-pb_sf%>%rename(ID=FID)
ap_sf <- ap_sf %>% rename(ID = X.1)
#mil_sf <- mil_sf %>% rename(ID = ID) Already set to ID
mil_sf$ID <- as.numeric(mil_sf$ID)
sfs_sf <- sfs_sf %>% rename(ID = X)
tri_sf <- tri_sf %>% rename(ID = X)

###########
#New Codes# 
###########

#### Resitrict to CA ####
ca_pb_sf<-pb_sf[pb_sf$STATE=="CA",]
ca_ap_sf=ap_sf[ap_sf$state_post_office_code=="CA",]
ca_mil_sf=mil_sf[mil_sf$STATE_CODE=="CA",]
ca_sfs_sf=sfs_sf[sfs_sf$STATE_CODE=="CA",]
ca_tri_sf=tri_sf[tri_sf$X8..ST=="CA",]

#### US Location Data #### 
#longitude and latitude information are stored in "geometry" feature
US_loc<-bind_rows(
  pb_sf%>%select(ID,geometry)%>%mutate(Type="pb"),
  sfs_sf%>%select(ID,geometry)%>%mutate(Type="sfs"),
ap_sf%>%select(ID,geometry)%>%mutate(Type="ap"),
mil_sf%>%select(ID,geometry)%>%mutate(Type="mil"),
tri_sf%>%select(ID,geometry)%>%mutate(Type="tri")
)%>%mutate(Type=factor(Type))


CA_loc<-bind_rows(
  ca_pb_sf%>%select(ID,geometry)%>%mutate(Type="pb"),
  ca_sfs_sf%>%select(ID,geometry)%>%mutate(Type="sfs"),
  ca_ap_sf%>%select(ID,geometry)%>%mutate(Type="ap"),
  ca_mil_sf%>%select(ID,geometry)%>%mutate(Type="mil"),
  ca_tri_sf%>%select(ID,geometry)%>%mutate(Type="tri")
)%>%mutate(Type=factor(Type))

###The location info are saved in local.rda file
save(US_loc,CA_loc,file = "data/location.rda")
rm(list=ls())
load("data/location.rda")
