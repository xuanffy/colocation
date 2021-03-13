#Ripley's cross-K functions
library(tidyverse)
library(spatstat)
library(sf)
library(sp)
library(rgdal)
library(ggmap)
library(maptools)
####CA Analysis ####
rm(list=ls())
load("data/location.rda")
ca.shape=readShapeSpatial("data/ca-state-boundary/CA_State_TIGER2016.shp")

#READ CA shapefile:
ca.shape=read_sf("data/ca-state-boundary/CA_State_TIGER2016.shp")
CA.shape.utm=st_transform(ca.shape, 32610) #Converted into UTM system
#converting data points into utm
CA.utm=st_transform(CA_loc, 32610) #Converted into UTM system
CA.utm=CA.utm[!st_is_empty(CA.utm),]
CA.km<-CA.utm%>%mutate(geometry=geometry/1000)#change into km

CA.sfs=CA.km%>%filter(Type=="sfs"|Type=="pb")
CA.ap=CA.km%>%filter(Type=="ap"|Type=="pb")
CA.mil=CA.km%>%filter(Type=="mil"|Type=="pb")
CA.tri=CA.km%>%filter(Type=="tri"|Type=="pb")
#ppp data-type setup####
CA.sfs.p=as.ppp(CA.sfs)
marks(CA.sfs.p)=factor(as.character(CA.sfs$Type))
CA.ap.p=as.ppp(CA.ap)
marks(CA.ap.p)=factor(as.character(CA.ap$Type))
CA.mil.p=as.ppp(CA.mil)
marks(CA.mil.p)=factor(as.character(CA.mil$Type))
CA.tri.p=as.ppp(CA.tri)
marks(CA.tri.p)=factor(as.character(CA.tri$Type))
#pairwise cross-K plot####
r=seq(0,500,by=0.4)
kcross.sfs=Kcross(CA.sfs.p,i="pb",j="sfs",r=r)[,-c(2:4)]
kcross.ap=Kcross(CA.ap.p,i="pb",j="ap",r=r)[,-c(2:4)]
kcross.mil=Kcross(CA.mil.p,i="pb",j="mil",r=r)[,-c(2:4)]
kcross.tri=Kcross(CA.tri.p,i="pb",j="tri",r=r)[,-c(2:4)]
kcross=cbind(kcross.sfs,kcross.ap,kcross.mil,kcross.tri)
colnames(kcross)=c("r","sfs","ap","mil","tri")
kcross$theo=pi*r^2
attr(kcross,"labl")=gsub("iso","obs",attr(kcross,"labl"))
attr(kcross,"labl")[6]="{K[theo](r)}"
plot(kcross,col=c(4,3,5,6,8),lty=c(2,rep(1,4)),ylab="K(r)",xlab="r (in km)",main="Ripley's K",xlim=c(0,500))
#L function transformation:
L.CA=kcross%>%mutate(
  sfs=sqrt(sfs/pi)-r,
  ap=sqrt(ap/pi)-r,mil=sqrt(mil/pi)-r,tri=sqrt(tri/pi)-r,
  theo=sqrt(theo/pi)-r
)
attr(L.CA,"labl")=gsub("K","L",attr(kcross,"labl"))
plot(L.CA,col=c(4,3,5,6,8),lty=c(2,rep(1,4)),ylab="L(r)",xlab="r (in km)",main="Rescale",legendpos="bottomright",xlim=c(0,500))
#pairwise cross-K envelope ####
set.seed(260)
sfs.shuffle <- expression(rlabel(CA.sfs.p))
ap.shuffle <- expression(rlabel(CA.ap.p))
mil.shuffle <- expression(rlabel(CA.mil.p))
tri.shuffle <- expression(rlabel(CA.tri.p))

e.sfs <- envelope(CA.sfs.p, Kcross, nsim=39, simulate=sfs.shuffle, i="pb", j="sfs",r=r,correction="best")
l.sfs <- envelope(CA.sfs.p, Lcross, nsim=39, simulate=sfs.shuffle, i="pb", j="sfs",r=r,correction="best")
e.ap <- envelope(CA.ap.p, Kcross, nsim=39, simulate=ap.shuffle, i="pb", j="ap",r=r,correction="best")
l.ap <- envelope(CA.ap.p, Lcross, nsim=39, simulate=ap.shuffle, i="pb", j="ap",r=r,correction="best")
e.mil <- envelope(CA.mil.p, Kcross, nsim=39, simulate=mil.shuffle, i="pb", j="mil",r=r,correction="best")
l.mil <- envelope(CA.mil.p, Lcross, nsim=39, simulate=mil.shuffle, i="pb", j="mil",r=r,correction="best")
e.tri <- envelope(CA.tri.p, Kcross, nsim=39, simulate=tri.shuffle, i="pb", j="tri",r=r,correction="best")
l.tri <- envelope(CA.tri.p, Lcross, nsim=39, simulate=tri.shuffle, i="pb", j="tri",r=r,correction="best")

plot(e.sfs,main="Superfund Sites",xlim=c(0,500))
lines(r,pi*r^2,col="blue",lty=2)
plot(e.ap,main="Airport",xlim=c(0,500))
lines(r,pi*r^2,col="blue",lty=2)
plot(e.mil,main="Military Bases",xlim=c(0,500))
lines(r,pi*r^2,col="blue",lty=2)
plot(e.tri,main="TRI Sites",xlim=c(0,500))
lines(r,pi*r^2,col="blue",lty=2)

par(mfrow=c(2,1),mai=c(1,1.2,0.4,0),mar=c(4,5,3,1)+.1,mex=0.8)
plot(e.sfs, . - mmean ~ r,main="Cross K",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(e.sfs$obs>e.sfs$lo)[1]],col="green3",lty=2)
text(r[which(e.sfs$obs>e.sfs$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))
plot(l.sfs, . - mmean ~ r,main="Cross L",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(l.sfs$obs>l.sfs$lo)[1]],col="green3",lty=2)
text(r[which(l.sfs$obs>l.sfs$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))

plot(e.ap, . - mmean ~ r,main="Cross K",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(e.ap$obs>e.ap$lo)[1]],col="green3",lty=2)
text(r[which(e.ap$obs>e.ap$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))
plot(l.ap, . - mmean ~ r,main="Cross L",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(l.ap$obs>l.ap$lo)[1]],col="green3",lty=2)
text(r[which(l.ap$obs>l.ap$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))



plot(e.mil, . - mmean ~ r,main="Cross K",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(e.mil$obs>e.mil$lo)[1]],col="green3",lty=2)
text(r[which(e.mil$obs>e.mil$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))
plot(l.mil, . - mmean ~ r,main="Cross L",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(l.mil$obs>l.mil$lo)[1]],col="green3",lty=2)
text(r[which(l.mil$obs>l.mil$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))


plot(e.tri, . - mmean ~ r,main="Cross K",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(e.tri$obs>e.tri$lo)[1]],col="green3",lty=2)
text(r[which(e.tri$obs>e.tri$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))
plot(l.tri, . - mmean ~ r,main="Cross L",legend=F,xlab="r (in km)",xlim=c(0,500))
abline(v=r[which(l.tri$obs>l.tri$lo)[1]],col="green3",lty=2)
text(r[which(l.tri$obs>l.tri$lo)[1]],0,paste0("r=",r[which(e.sfs$obs>e.sfs$lo)[1]]))






