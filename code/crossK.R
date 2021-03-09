#Ripley's cross-K functions
library(tidyverse)
library(spatstat)
library(sf)
library(sp)
library(rgdal)
library(ggmap)
####US Analysis####
rm(list=ls())
load("data/location.rda")

# US_loc<-US_loc%>%mutate(Type=factor(Type))
#                 lat = unlist(map(geometry,1)),
#                 long = unlist(map(geometry,2)))

#ppp data-type setup####
#Converted into UTM system
US.utm=st_transform(US_loc, 32619) 
US.utm=US.utm[!st_is_empty(US.utm),]
US.utm.km<-US.utm%>%mutate(geometry=geometry/1000)#change into km
US.ppp=as.ppp(US.utm.km)
marks(US.ppp)=US.utm.km$Type
#cross-K plot ####
#for SFS:
US.sfs.kcross=Kcross(US.ppp,"pb","sfs")
plot(US.sfs.kcross)
US.ap.kcross=Kcross(US.ppp,"pb","ap")
#for airport data
plot(US.ap.kcross)
#Monte Carlo Envelopes####
US.shuffle <- expression(rlabel(US.ppp))# Random labelling test
US.sfs.e <- envelope(US.ppp, Kcross, nsim=49, simulate=US.shuffle, i="sfs", j="pb")
plot(US.sfs.e)
US.ap.e=envelope(US.ppp, Kcross, nsim=49, simulate=us.shuffle, i="pb", j="ap")#Monte Carlo Envelopes
plot(US.ap.e)

####CA Analysis ####
#ppp data-type setup####
CA.utm=st_transform(CA_loc, 32619) #Converted into UTM system
CA.utm=CA.utm[!st_is_empty(CA.utm),]
CA.utm.km<-CA.utm%>%mutate(geometry=geometry/1000)#change into km
CA.ppp=as.ppp(CA.utm.km)
marks(CA.ppp)=CA.utm.km$Type
#cross-K plot####
CA.sfs.kcross=Kcross(CA.ppp,"pb","sfs")
plot(CA.sfs.kcross)
CA.ap.kcross=Kcross(CA.ppp,"pb","ap")
#for airport data
plot(CA.ap.kcross)
#Monte Carlo Envelopes####
CA.shuffle <- expression(rlabel(CA.ppp))# Random labelling test
CA.sfs.e <- envelope(CA.ppp, Kcross, nsim=99, simulate=CA.shuffle, i="pb", j="sfs",correction="Ripley",verbose = F)#Monte Carlo Envelopes
plot(CA.sfs.e)
CA.ap.e=envelope(CA.ppp, Kcross, nsim=99, simulate=CA.shuffle, i="pb", j="ap")#Monte Carlo Envelopes
plot(CA.ap.e)


