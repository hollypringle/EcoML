# Diurnal/Nocturnal####


#Find midpoint coordinates of study area to find sunrise/sunset time
library(maptools)
library(rgeos)
library(sf)
library(rgdal)
library(raster)
library(rgdal)
library(dplyr)
library(maptools)
library(ggplot2)

coords<-read.csv("~/Biome_Health_Project/Data/coordinates_allsites.csv")
head(coords)
coords$longitude <- as.numeric(as.character(coords$longitude ))
coords$latitude  <- as.numeric(as.character(coords$latitude ))

names(coords)
coords$X<-NULL
coords$CT_site<-NULL
coords$conservancy<-NULL


center_point <- centroid(coords)
##lat      long
#-1.338654 35.17827


# 20th November 2018 - https://gml.noaa.gov/grad/solcalc/
#sunrise 6:19
#sunset 18:30

###gt####

baboonObs_gt<-gt_raw[gt_raw$label == "baboon", "time_rad"]
baboonDens_gt <- densityPlot(baboonObs_gt, extend=NULL)
# What proportion of the density lies between 6:19 and 18:30 hrs?
wanted <- baboonDens_gt $x > 6.31666666666 & baboonDens_gt $x < 18.5
baboon_diurnal_gt<-mean(baboonDens_gt$y[wanted]) * 12.18333 # probability mass for the ~12 hr period.
# Plotting time in radians:
densityPlot(baboonObs_gt, xscale=NA, rug=TRUE)
baboon_diurnal_gt

buffaloObs_gt<-gt_raw[gt_raw$label == "buffalo", "time_rad"]
buffaloDens_gt <- densityPlot(buffaloObs_gt, extend=NULL)
# What proportion of the density lies between 6:19 and 18:30 hrs?
wanted <- buffaloDens_gt $x > 6.31666666666 & buffaloDens_gt $x < 18.5
buffalo_diurnal_gt<-mean(buffaloDens_gt$y[wanted]) * 12.18333 # probability mass for the ~12 hr period.
# Plotting time in radians:
densityPlot(buffaloObs_gt, xscale=NA, rug=TRUE)


dikdikObs_gt<-gt_raw[gt_raw$label == "dikdik", "time_rad"]
dikdikDens_gt <- densityPlot(dikdikObs_gt, extend=NULL)
# What proportion of the density lies between 6:19 and 18:30 hrs?
wanted <- dikdikDens_gt $x > 6.31666666666 & dikdikDens_gt $x < 18.5
dikdik_diurnal_gt<-mean(dikdikDens_gt$y[wanted]) * 12.18333 # probability mass for the ~12 hr period.
# Plotting time in radians:
densityPlot(dikdikObs_gt, xscale=NA, rug=TRUE)
