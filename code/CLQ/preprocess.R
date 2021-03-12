library(tidyverse)
library(sf)
library(rgeos)
library(foreach)
library(doParallel)
library(lwgeom)

pb <- st_read("data-clean/Prison_Boundaries.shp", stringsAsFactors = FALSE)

#Convert prisons to match (larger) FRS fac data set Coordinate Reference System
pb_sf <- st_transform(pb, crs = 4269)

#Reduce prisons from polygons to points (centroids) to reduces distance calculation times
pb_sf <- st_transform(pb_sf, crs = 32617) %>% #convert to utm for calculating centroids
  st_centroid() %>% #centroids from original multipolygons
  st_transform(crs = 4269) #back to 4269

pb_crs <- st_crs(pb_sf) #get the CRS for prison centroids

#Read airports data file and convert to sf
ap <- read.csv("data-clean/airports.csv", stringsAsFactors = FALSE) 
ap_sf <- st_as_sf(ap, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read military bases data file
mil <- st_read("data-clean/military_bases.csv", stringsAsFactors = FALSE) 
mil_sf <- st_as_sf(mil, coords = c("X", "Y"), crs = pb_crs, na.fail = FALSE)

#Read superfund sites data file and convert to sf
sfs <- read.csv("data-clean/sf.csv", stringsAsFactors = FALSE) 
sfs <- sfs %>% filter(!is.na(LONGITUDE83))
sfs_sf <- st_as_sf(sfs, coords = c("LONGITUDE83", "LATITUDE83"), crs = pb_crs, na.fail = FALSE)

#Read TRI sites data file and convert to sf
tri <- read.csv("data-clean/tri.csv", stringsAsFactors = FALSE) 
tri <- tri %>% filter(!is.na(X12..LATITUDE))
tri_sf <- st_as_sf(tri, coords = c("X13..LONGITUDE", "X12..LATITUDE"), crs = pb_crs, na.fail = FALSE)

#Rename ID columns so they all match
ap_sf <- ap_sf %>% rename(ID = X.1)
#mil_sf <- mil_sf %>% rename(ID = ID) Already set to ID
mil_sf$ID <- as.numeric(mil_sf$ID)
sfs_sf <- sfs_sf %>% rename(ID = X)
tri_sf <- tri_sf %>% rename(ID = X)

#########################
# New Code
#########################
# only extract prisons, airports, military bases, superfund sites, TRI sites in CA
ca_pb_sf <- pb_sf[pb_sf$STATE == "CA", ]
ca_ap_sf <- ap_sf[ap_sf$state_post_office_code == "CA", ]
ca_mil_sf <- mil_sf[mil_sf$STATE_CODE == "CA", ]
ca_sfs_sf <- sfs_sf[sfs_sf$STATE_CODE == "CA", ]
ca_tri_sf <- tri_sf[tri_sf$X8..ST == "CA", ]

n1 <- nrow(ca_pb_sf) 
n2 <- nrow(ca_ap_sf)
n3 <- nrow(ca_mil_sf) 
n4 <- nrow(ca_sfs_sf) 
n5 <- nrow(ca_tri_sf)
n <- n1 + n2 + n3 + n4 + n5

sites_to_list <- function(sf) {
  output <- list()
  for (k in seq(1, nrow(sf))) {
    output[[k]] <- sf[k, ]
  }
  return(output)
}

all_sites <- c(sites_to_list(ca_pb_sf),
               sites_to_list(ca_ap_sf),
               sites_to_list(ca_mil_sf),
               sites_to_list(ca_sfs_sf),
               sites_to_list(ca_tri_sf))
id_list <- seq(1, n)
# 1 - prison
# 2 - aiport
# 3 - military base
# 4 - superfund site
# 5 - TRI site
type_list <- vector(mode = 'character', length = n)
type_list[1:n1] <- rep("pb", n1)
type_list[(n1+1):(n1+n2)] <- rep("ap", n2)
type_list[(n1+n2+1):(n1+n2+n3)] <- rep("mil", n3)
type_list[(n1+n2+n3+1):(n1+n2+n3+n4)] <- rep("sfs", n4)
type_list[(n1+n2+n3+n4+1):(n1+n2+n3+n4+n5)] <- rep("tri", n5)


# pair_dist <- matrix(-1, nrow = n, ncol = n)
# # start_time <- Sys.time()
# for (i in seq(1, n)) {
#   for (j in seq(i, n)) {
#     pair_dist[i,j] <- st_distance(all_sites[[i]], all_sites[[j]])
#     # st_distance(ca_pb_sf[1, ], ca_pb_sf[3, ])
#   }
# }
# # end_time <- Sys.time()
# # print(as.numeric(end_time - start_time, units = "secs"))

n <- 100
# use parallel computing
ncores <- 1
registerDoParallel(ncores)
start_time <- Sys.time()
r <- foreach(i = 1 : ncores, .combine=rbind) %dopar% {
  library(sf)
  library(rgeos)
  block <- ceiling( n / ncores )
  if (i == ncores) {
    dist_block <- matrix(-1, nrow = n - block * (ncores - 1), ncol = n)
  } else{
    dist_block <- matrix(-1, nrow = block, ncol = n)  
  }
  
  for (k in seq(1, nrow(dist_block)) ) {
    for (j in seq(1, n))
    dist_block[k, j] <- st_distance(all_sites[[(i-1)*block + k]], all_sites[[j]])
  }
  print(i)
  dist_block
}

end_time <- Sys.time()
print(as.numeric(end_time - start_time, units = "secs"))

# write.csv(r, file = 'CA_sites_pair.csv')


print(paste("n1 = ", n1))
print(paste("n2 = ", n2))
print(paste("n3 = ", n3))
print(paste("n4 = ", n4))
print(paste("n5 = ", n5))

stopImplicitCluster()