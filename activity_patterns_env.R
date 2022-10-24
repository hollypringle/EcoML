#Load packages####
library(activity)
library(overlap)
library(beepr)
library(circular)
library(data.table)

#Setup####
gt_raw<-read.csv("kenya_ecoml_labels_Manual.csv")
head(gt_raw)

ResNet18_raw<-read.csv("kenya_ecoml_labels_ResNet18.csv")
head(ResNet18_raw)

ResNet50_raw<-read.csv("kenya_ecoml_labels_ResNet50.csv")
head(ResNet50_raw)

ConvNextT_raw<-read.csv("kenya_ecoml_labels_ConvNextT.csv")
head(ConvNextT_raw)

gt_raw$time_rad <- gettime(gt_raw$datetime, "%Y-%m-%d %H:%M:%S", "radian")  # transforming time in radians
gt_raw$time_prop <- gettime(gt_raw$datetime, "%Y-%m-%d %H:%M:%S","proportion") # transforming time in proportion
range(gt_raw$time_rad) # just checking - should be between ca. 0 and 6.28
range(gt_raw$time_prop) # just checking - should be between 0 and 1
nrow(gt_raw) #23319

ResNet18_raw$time_rad <- gettime(ResNet18_raw$datetime, "%Y-%m-%d %H:%M:%S", "radian")  # transforming time in radians
ResNet18_raw$time_prop <- gettime(ResNet18_raw$datetime, "%Y-%m-%d %H:%M:%S","proportion") # transforming time in proportion
range(ResNet18_raw$time_rad) # just checking - should be between ca. 0 and 6.28
range(ResNet18_raw$time_prop) # just checking - should be between 0 and 1
nrow(ResNet18_raw) #23319

ResNet50_raw$time_rad <- gettime(ResNet50_raw$datetime, "%Y-%m-%d %H:%M:%S", "radian")  # transforming time in radians
ResNet50_raw$time_prop <- gettime(ResNet50_raw$datetime, "%Y-%m-%d %H:%M:%S","proportion") # transforming time in proportion
range(ResNet50_raw$time_rad) # just checking - should be between ca. 0 and 6.28
range(ResNet50_raw$time_prop) # just checking - should be between 0 and 1
nrow(ResNet50_raw) #23319

ConvNextT_raw$time_rad <- gettime(ConvNextT_raw$datetime, "%Y-%m-%d %H:%M:%S", "radian")  # transforming time in radians
ConvNextT_raw$time_prop <- gettime(ConvNextT_raw$datetime, "%Y-%m-%d %H:%M:%S","proportion") # transforming time in proportion
range(ConvNextT_raw$time_rad) # just checking - should be between ca. 0 and 6.28
range(ConvNextT_raw$time_prop) # just checking - should be between 0 and 1
nrow(ConvNextT_raw) #23319

# SOLAR TIME FUNCTIONS FROM ROWCLIFFE ###
require(insol)

gettime <- function(x, format="%Y-%m-%d %H:%M:%S", scale=c("radian","hour","proportion")){
  if(class(x)[1]=="character") x <- strptime(x, format, "UTC") else
    if(class(x)[1]=="POSIXct") x <- as.POSIXlt(x) else
      if(class(x)[1]!="POSIXlt") stop("x must be character or POSIXt class")
    scale <- match.arg(scale)
    res <- x$hour + x$min/60 + x$sec/3600
    if(scale=="radian") res <- res*pi/12
    if(scale=="proportion") res <- res/24
    if(all(res==0, na.rm=T)) warning("All times are 0: may be just strptime default?")
    res
}

##aardvark #7 gt records- don't run ####


##GT
aardvark_length_gt_raw<-length(gt_raw[gt_raw$label == "aardvark", "time_rad"]) #519
#7
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_aardvark<- fitact(gt_raw[gt_raw$label == "aardvark", "time_rad"],
                           reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_aardvark, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "aardvark", cex.main=1)
aardvark_gt_act<-as.data.frame(gt_raw_aardvark@act) # view estimates of proportion of time active

##ResNet18 raw
aardvark_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "aardvark", "time_rad"])
ResNet18_raw_aardvark<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "aardvark", "time_rad"],
                                 reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
aardvark_ResNet18_act<-as.data.frame(ResNet18_raw_aardvark@act)

##ResNet50 raw
aardvark_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "aardvark", "time_rad"])
ResNet50_raw_aardvark<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "aardvark", "time_rad"],
                                 reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
aardvark_ResNet50_act<-as.data.frame(ResNet50_raw_aardvark@act)

##ConvNextT 90%
aardvark_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "aardvark", "time_rad"]) #519
ConvNextT_raw_aardvark<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "aardvark", "time_rad"],
                                  reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
aardvark_ConvNextT_act<-as.data.frame(ConvNextT_raw_aardvark@act)

aardvark_gt_act<-as.data.frame(t(aardvark_gt_act))
aardvark_gt_act$n<-aardvark_length_gt_raw
aardvark_ResNet18_act<-as.data.frame(t(aardvark_ResNet18_act))
aardvark_ResNet18_act$n<-aardvark_length_ResNet18_raw
aardvark_ResNet50_act<-as.data.frame(t(aardvark_ResNet50_act))
aardvark_ResNet50_act$n<-aardvark_length_ResNet50_raw
aardvark_ConvNextT_act<-as.data.frame(t(aardvark_ConvNextT_act))
aardvark_ConvNextT_act$n<-aardvark_length_ConvNextT_raw

##baboon #77 gt records - done ####

##GT
baboon_length_gt_raw<-length(gt_raw[gt_raw$label == "baboon", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_baboon<- fitact(gt_raw[gt_raw$label == "baboon", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_baboon, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "baboon", cex.main=1)
baboon_gt_act<-as.data.frame(gt_raw_baboon@act) # view estimates of proportion of time active

#ResNet18 raw
baboon_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "baboon", "time_rad"])
ResNet18_raw_baboon<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "baboon", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
baboon_ResNet18_act<-as.data.frame(ResNet18_raw_baboon@act)

#ResNet50 raw
baboon_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "baboon", "time_rad"])
ResNet50_raw_baboon<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "baboon", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
baboon_ResNet50_act<-as.data.frame(ResNet50_raw_baboon@act)

#ConvNextT 90%
baboon_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "baboon", "time_rad"]) #519
ConvNextT_raw_baboon<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "baboon", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
baboon_ConvNextT_act<-as.data.frame(ConvNextT_raw_baboon@act)

baboon_gt_act<-as.data.frame(t(baboon_gt_act))
baboon_gt_act$n<-baboon_length_gt_raw
baboon_ResNet18_act<-as.data.frame(t(baboon_ResNet18_act))
baboon_ResNet18_act$n<-baboon_length_ResNet18_raw
baboon_ResNet50_act<-as.data.frame(t(baboon_ResNet50_act))
baboon_ResNet50_act$n<-baboon_length_ResNet50_raw
baboon_ConvNextT_act<-as.data.frame(t(baboon_ConvNextT_act))
baboon_ConvNextT_act$n<-baboon_length_ConvNextT_raw

##bateared_fox #22 gt records- don't run####

##GT
bateared_fox_length_gt_raw<-length(gt_raw[gt_raw$label == "bateared_fox", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_bateared_fox<- fitact(gt_raw[gt_raw$label == "bateared_fox", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_bateared_fox, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "bateared_fox", cex.main=1)
bateared_fox_gt_act<-as.data.frame(gt_raw_bateared_fox@act) # view estimates of proportion of time active

#ResNet18 raw
bateared_fox_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "bateared_fox", "time_rad"])
ResNet18_raw_bateared_fox<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "bateared_fox", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
bateared_fox_ResNet18_act<-as.data.frame(ResNet18_raw_bateared_fox@act)

#ResNet50 raw
bateared_fox_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "bateared_fox", "time_rad"])
ResNet50_raw_bateared_fox<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "bateared_fox", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
bateared_fox_ResNet50_act<-as.data.frame(ResNet50_raw_bateared_fox@act)

#ConvNextT 90%
bateared_fox_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "bateared_fox", "time_rad"]) #519
ConvNextT_raw_bateared_fox<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "bateared_fox", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
bateared_fox_ConvNextT_act<-as.data.frame(ConvNextT_raw_bateared_fox@act)

bateared_fox_gt_act<-as.data.frame(t(bateared_fox_gt_act))
bateared_fox_gt_act$n<-bateared_fox_length_gt_raw
bateared_fox_ResNet18_act<-as.data.frame(t(bateared_fox_ResNet18_act))
bateared_fox_ResNet18_act$n<-bateared_fox_length_ResNet18_raw
bateared_fox_ResNet50_act<-as.data.frame(t(bateared_fox_ResNet50_act))
bateared_fox_ResNet50_act$n<-bateared_fox_length_ResNet50_raw
bateared_fox_ConvNextT_act<-as.data.frame(t(bateared_fox_ConvNextT_act))
bateared_fox_ConvNextT_act$n<-bateared_fox_length_ConvNextT_raw

##buffalo #194 gt records - done####

##GT
buffalo_length_gt_raw<-length(gt_raw[gt_raw$label == "buffalo", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_buffalo<- fitact(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_buffalo, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "buffalo", cex.main=1)
buffalo_gt_act<-as.data.frame(gt_raw_buffalo@act) # view estimates of proportion of time active

#ResNet18 raw
buffalo_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "buffalo", "time_rad"])
ResNet18_raw_buffalo<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "buffalo", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
buffalo_ResNet18_act<-as.data.frame(ResNet18_raw_buffalo@act)

#ResNet50 raw
buffalo_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "buffalo", "time_rad"])
ResNet50_raw_buffalo<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "buffalo", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
buffalo_ResNet50_act<-as.data.frame(ResNet50_raw_buffalo@act)

#ConvNextT 90%
buffalo_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "buffalo", "time_rad"]) #519
ConvNextT_raw_buffalo<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "buffalo", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
buffalo_ConvNextT_act<-as.data.frame(ConvNextT_raw_buffalo@act)

buffalo_gt_act<-as.data.frame(t(buffalo_gt_act))
buffalo_gt_act$n<-buffalo_length_gt_raw
buffalo_ResNet18_act<-as.data.frame(t(buffalo_ResNet18_act))
buffalo_ResNet18_act$n<-buffalo_length_ResNet18_raw
buffalo_ResNet50_act<-as.data.frame(t(buffalo_ResNet50_act))
buffalo_ResNet50_act$n<-buffalo_length_ResNet50_raw
buffalo_ConvNextT_act<-as.data.frame(t(buffalo_ConvNextT_act))
buffalo_ConvNextT_act$n<-buffalo_length_ConvNextT_raw
##bushbuck #4 gt records-dont run####

##GT
bushbuck_length_gt_raw<-length(gt_raw[gt_raw$label == "bushbuck", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_bushbuck<- fitact(gt_raw[gt_raw$label == "bushbuck", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_bushbuck, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "bushbuck", cex.main=1)
bushbuck_gt_act<-as.data.frame(gt_raw_bushbuck@act) # view estimates of proportion of time active

#ResNet18 raw
bushbuck_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "bushbuck", "time_rad"])
ResNet18_raw_bushbuck<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "bushbuck", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
bushbuck_ResNet18_act<-as.data.frame(ResNet18_raw_bushbuck@act)

#ResNet50 raw
bushbuck_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "bushbuck", "time_rad"])
ResNet50_raw_bushbuck<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "bushbuck", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
bushbuck_ResNet50_act<-as.data.frame(ResNet50_raw_bushbuck@act)

#ConvNextT 90%
bushbuck_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "bushbuck", "time_rad"]) #519
ConvNextT_raw_bushbuck<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "bushbuck", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
bushbuck_ConvNextT_act<-as.data.frame(ConvNextT_raw_bushbuck@act)

bushbuck_gt_act<-as.data.frame(t(bushbuck_gt_act))
bushbuck_gt_act$n<-bushbuck_length_gt_raw
bushbuck_ResNet18_act<-as.data.frame(t(bushbuck_ResNet18_act))
bushbuck_ResNet18_act$n<-bushbuck_length_ResNet18_raw
bushbuck_ResNet50_act<-as.data.frame(t(bushbuck_ResNet50_act))
bushbuck_ResNet50_act$n<-bushbuck_length_ResNet50_raw
bushbuck_ConvNextT_act<-as.data.frame(t(bushbuck_ConvNextT_act))
bushbuck_ConvNextT_act$n<-bushbuck_length_ConvNextT_raw
##cheetah #5 gt records-dont run####

##GT
cheetah_length_gt_raw<-length(gt_raw[gt_raw$label == "cheetah", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_cheetah<- fitact(gt_raw[gt_raw$label == "cheetah", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_cheetah, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "cheetah", cex.main=1)
cheetah_gt_act<-as.data.frame(gt_raw_cheetah@act) # view estimates of proportion of time active

#ResNet18 raw
cheetah_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "cheetah", "time_rad"])
ResNet18_raw_cheetah<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "cheetah", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
cheetah_ResNet18_act<-as.data.frame(ResNet18_raw_cheetah@act)

#ResNet50 raw
cheetah_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "cheetah", "time_rad"])
ResNet50_raw_cheetah<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "cheetah", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
cheetah_ResNet50_act<-as.data.frame(ResNet50_raw_cheetah@act)

#ConvNextT 90%
cheetah_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "cheetah", "time_rad"]) #519
ConvNextT_raw_cheetah<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "cheetah", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
cheetah_ConvNextT_act<-as.data.frame(ConvNextT_raw_cheetah@act)

cheetah_gt_act<-as.data.frame(t(cheetah_gt_act))
cheetah_gt_act$n<-cheetah_length_gt_raw
cheetah_ResNet18_act<-as.data.frame(t(cheetah_ResNet18_act))
cheetah_ResNet18_act$n<-cheetah_length_ResNet18_raw
cheetah_ResNet50_act<-as.data.frame(t(cheetah_ResNet50_act))
cheetah_ResNet50_act$n<-cheetah_length_ResNet50_raw
cheetah_ConvNextT_act<-as.data.frame(t(cheetah_ConvNextT_act))
cheetah_ConvNextT_act$n<-cheetah_length_ConvNextT_raw
##dikdik #154 gt records- done####

##GT
dikdik_length_gt_raw<-length(gt_raw[gt_raw$label == "dikdik", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_dikdik<- fitact(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_dikdik, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "dikdik", cex.main=1)
dikdik_gt_act<-as.data.frame(gt_raw_dikdik@act) # view estimates of proportion of time active

#ResNet18 raw
dikdik_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "dikdik", "time_rad"])
ResNet18_raw_dikdik<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "dikdik", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
dikdik_ResNet18_act<-as.data.frame(ResNet18_raw_dikdik@act)

#ResNet50 raw
dikdik_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "dikdik", "time_rad"])
ResNet50_raw_dikdik<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "dikdik", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
dikdik_ResNet50_act<-as.data.frame(ResNet50_raw_dikdik@act)

#ConvNextT 90%
dikdik_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "dikdik", "time_rad"]) #519
ConvNextT_raw_dikdik<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "dikdik", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
dikdik_ConvNextT_act<-as.data.frame(ConvNextT_raw_dikdik@act)

dikdik_gt_act<-as.data.frame(t(dikdik_gt_act))
dikdik_gt_act$n<-dikdik_length_gt_raw
dikdik_ResNet18_act<-as.data.frame(t(dikdik_ResNet18_act))
dikdik_ResNet18_act$n<-dikdik_length_ResNet18_raw
dikdik_ResNet50_act<-as.data.frame(t(dikdik_ResNet50_act))
dikdik_ResNet50_act$n<-dikdik_length_ResNet50_raw
dikdik_ConvNextT_act<-as.data.frame(t(dikdik_ConvNextT_act))
dikdik_ConvNextT_act$n<-dikdik_length_ConvNextT_raw
##eland #103 gt records - done####

##GT
eland_length_gt_raw<-length(gt_raw[gt_raw$label == "eland", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_eland<- fitact(gt_raw[gt_raw$label == "eland", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_eland, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "eland", cex.main=1)
eland_gt_act<-as.data.frame(gt_raw_eland@act) # view estimates of proportion of time active

#ResNet18 raw
eland_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "eland", "time_rad"])
ResNet18_raw_eland<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "eland", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
eland_ResNet18_act<-as.data.frame(ResNet18_raw_eland@act)

#ResNet50 raw
eland_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "eland", "time_rad"])
ResNet50_raw_eland<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "eland", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
eland_ResNet50_act<-as.data.frame(ResNet50_raw_eland@act)

#ConvNextT 90%
eland_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "eland", "time_rad"]) #519
ConvNextT_raw_eland<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "eland", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
eland_ConvNextT_act<-as.data.frame(ConvNextT_raw_eland@act)

eland_gt_act<-as.data.frame(t(eland_gt_act))
eland_gt_act$n<-eland_length_gt_raw
eland_ResNet18_act<-as.data.frame(t(eland_ResNet18_act))
eland_ResNet18_act$n<-eland_length_ResNet18_raw
eland_ResNet50_act<-as.data.frame(t(eland_ResNet50_act))
eland_ResNet50_act$n<-eland_length_ResNet50_raw
eland_ConvNextT_act<-as.data.frame(t(eland_ConvNextT_act))
eland_ConvNextT_act$n<-eland_length_ConvNextT_raw
##elephant #229 gt records- done####

##GT
elephant_length_gt_raw<-length(gt_raw[gt_raw$label == "elephant", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_elephant<- fitact(gt_raw[gt_raw$label == "elephant", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_elephant, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "elephant", cex.main=1)
elephant_gt_act<-as.data.frame(gt_raw_elephant@act) # view estimates of proportion of time active

#ResNet18 raw
elephant_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "elephant", "time_rad"])
ResNet18_raw_elephant<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "elephant", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
elephant_ResNet18_act<-as.data.frame(ResNet18_raw_elephant@act)

#ResNet50 raw
elephant_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "elephant", "time_rad"])
ResNet50_raw_elephant<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "elephant", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
elephant_ResNet50_act<-as.data.frame(ResNet50_raw_elephant@act)

#ConvNextT 90%
elephant_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "elephant", "time_rad"]) #519
ConvNextT_raw_elephant<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "elephant", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
elephant_ConvNextT_act<-as.data.frame(ConvNextT_raw_elephant@act)

elephant_gt_act<-as.data.frame(t(elephant_gt_act))
elephant_gt_act$n<-elephant_length_gt_raw
elephant_ResNet18_act<-as.data.frame(t(elephant_ResNet18_act))
elephant_ResNet18_act$n<-elephant_length_ResNet18_raw
elephant_ResNet50_act<-as.data.frame(t(elephant_ResNet50_act))
elephant_ResNet50_act$n<-elephant_length_ResNet50_raw
elephant_ConvNextT_act<-as.data.frame(t(elephant_ConvNextT_act))
elephant_ConvNextT_act$n<-elephant_length_ConvNextT_raw
##gazelle_grants #152 gt records - done####

##GT
gazelle_grants_length_gt_raw<-length(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_gazelle_grants<- fitact(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_gazelle_grants, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "gazelle_grants", cex.main=1)
gazelle_grants_gt_act<-as.data.frame(gt_raw_gazelle_grants@act) # view estimates of proportion of time active

#ResNet18 raw
gazelle_grants_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_grants", "time_rad"])
ResNet18_raw_gazelle_grants<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_grants", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
gazelle_grants_ResNet18_act<-as.data.frame(ResNet18_raw_gazelle_grants@act)

#ResNet50 raw
gazelle_grants_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_grants", "time_rad"])
ResNet50_raw_gazelle_grants<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_grants", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
gazelle_grants_ResNet50_act<-as.data.frame(ResNet50_raw_gazelle_grants@act)

#ConvNextT 90%
gazelle_grants_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_grants", "time_rad"]) #519
ConvNextT_raw_gazelle_grants<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_grants", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
gazelle_grants_ConvNextT_act<-as.data.frame(ConvNextT_raw_gazelle_grants@act)

gazelle_grants_gt_act<-as.data.frame(t(gazelle_grants_gt_act))
gazelle_grants_gt_act$n<-gazelle_grants_length_gt_raw
gazelle_grants_ResNet18_act<-as.data.frame(t(gazelle_grants_ResNet18_act))
gazelle_grants_ResNet18_act$n<-gazelle_grants_length_ResNet18_raw
gazelle_grants_ResNet50_act<-as.data.frame(t(gazelle_grants_ResNet50_act))
gazelle_grants_ResNet50_act$n<-gazelle_grants_length_ResNet50_raw
gazelle_grants_ConvNextT_act<-as.data.frame(t(gazelle_grants_ConvNextT_act))
gazelle_grants_ConvNextT_act$n<-gazelle_grants_length_ConvNextT_raw
##gazelle_thomsons #2611 gt records - done####

##GT
gazelle_thomsons_length_gt_raw<-length(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_gazelle_thomsons<- fitact(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_gazelle_thomsons, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "gazelle_thomsons", cex.main=1)
gazelle_thomsons_gt_act<-as.data.frame(gt_raw_gazelle_thomsons@act) # view estimates of proportion of time active

#ResNet18 raw
gazelle_thomsons_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_thomsons", "time_rad"])
ResNet18_raw_gazelle_thomsons<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_thomsons", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
gazelle_thomsons_ResNet18_act<-as.data.frame(ResNet18_raw_gazelle_thomsons@act)

#ResNet50 raw
gazelle_thomsons_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_thomsons", "time_rad"])
ResNet50_raw_gazelle_thomsons<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_thomsons", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
gazelle_thomsons_ResNet50_act<-as.data.frame(ResNet50_raw_gazelle_thomsons@act)

#ConvNextT 90%
gazelle_thomsons_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_thomsons", "time_rad"]) #519
ConvNextT_raw_gazelle_thomsons<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_thomsons", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
gazelle_thomsons_ConvNextT_act<-as.data.frame(ConvNextT_raw_gazelle_thomsons@act)

gazelle_thomsons_gt_act<-as.data.frame(t(gazelle_thomsons_gt_act))
gazelle_thomsons_gt_act$n<-gazelle_thomsons_length_gt_raw
gazelle_thomsons_ResNet18_act<-as.data.frame(t(gazelle_thomsons_ResNet18_act))
gazelle_thomsons_ResNet18_act$n<-gazelle_thomsons_length_ResNet18_raw
gazelle_thomsons_ResNet50_act<-as.data.frame(t(gazelle_thomsons_ResNet50_act))
gazelle_thomsons_ResNet50_act$n<-gazelle_thomsons_length_ResNet50_raw
gazelle_thomsons_ConvNextT_act<-as.data.frame(t(gazelle_thomsons_ConvNextT_act))
gazelle_thomsons_ConvNextT_act$n<-gazelle_thomsons_length_ConvNextT_raw
##genet #13 GT records- dont run####

##GT
genet_length_gt_raw<-length(gt_raw[gt_raw$label == "genet", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_genet<- fitact(gt_raw[gt_raw$label == "genet", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_genet, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "genet", cex.main=1)
genet_gt_act<-as.data.frame(gt_raw_genet@act) # view estimates of proportion of time active

#ResNet18 raw
genet_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "genet", "time_rad"])
ResNet18_raw_genet<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "genet", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
genet_ResNet18_act<-as.data.frame(ResNet18_raw_genet@act)

#ResNet50 raw
genet_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "genet", "time_rad"])
ResNet50_raw_genet<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "genet", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
genet_ResNet50_act<-as.data.frame(ResNet50_raw_genet@act)

#ConvNextT 90%
genet_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "genet", "time_rad"]) #519
ConvNextT_raw_genet<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "genet", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
genet_ConvNextT_act<-as.data.frame(ConvNextT_raw_genet@act)

genet_gt_act<-as.data.frame(t(genet_gt_act))
genet_gt_act$n<-genet_length_gt_raw
genet_ResNet18_act<-as.data.frame(t(genet_ResNet18_act))
genet_ResNet18_act$n<-genet_length_ResNet18_raw
genet_ResNet50_act<-as.data.frame(t(genet_ResNet50_act))
genet_ResNet50_act$n<-genet_length_ResNet50_raw
genet_ConvNextT_act<-as.data.frame(t(genet_ConvNextT_act))
genet_ConvNextT_act$n<-genet_length_ConvNextT_raw
##giraffe #256 GT records - done####

##GT
giraffe_length_gt_raw<-length(gt_raw[gt_raw$label == "giraffe", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_giraffe<- fitact(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_giraffe, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "giraffe", cex.main=1)
giraffe_gt_act<-as.data.frame(gt_raw_giraffe@act) # view estimates of proportion of time active

#ResNet18 raw
giraffe_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "giraffe", "time_rad"])
ResNet18_raw_giraffe<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "giraffe", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
giraffe_ResNet18_act<-as.data.frame(ResNet18_raw_giraffe@act)

#ResNet50 raw
giraffe_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "giraffe", "time_rad"])
ResNet50_raw_giraffe<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "giraffe", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
giraffe_ResNet50_act<-as.data.frame(ResNet50_raw_giraffe@act)

#ConvNextT 90%
giraffe_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "giraffe", "time_rad"]) #519
ConvNextT_raw_giraffe<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "giraffe", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
giraffe_ConvNextT_act<-as.data.frame(ConvNextT_raw_giraffe@act)

giraffe_gt_act<-as.data.frame(t(giraffe_gt_act))
giraffe_gt_act$n<-giraffe_length_gt_raw
giraffe_ResNet18_act<-as.data.frame(t(giraffe_ResNet18_act))
giraffe_ResNet18_act$n<-giraffe_length_ResNet18_raw
giraffe_ResNet50_act<-as.data.frame(t(giraffe_ResNet50_act))
giraffe_ResNet50_act$n<-giraffe_length_ResNet50_raw
giraffe_ConvNextT_act<-as.data.frame(t(giraffe_ConvNextT_act))
giraffe_ConvNextT_act$n<-giraffe_length_ConvNextT_raw
##hare #161 GT records- done####

##GT
hare_length_gt_raw<-length(gt_raw[gt_raw$label == "hare", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_hare<- fitact(gt_raw[gt_raw$label == "hare", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_hare, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "hare", cex.main=1)
hare_gt_act<-as.data.frame(gt_raw_hare@act) # view estimates of proportion of time active

#ResNet18 raw
hare_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "hare", "time_rad"])
ResNet18_raw_hare<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "hare", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hare_ResNet18_act<-as.data.frame(ResNet18_raw_hare@act)

#ResNet50 raw
hare_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "hare", "time_rad"])
ResNet50_raw_hare<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "hare", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hare_ResNet50_act<-as.data.frame(ResNet50_raw_hare@act)

#ConvNextT 90%
hare_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "hare", "time_rad"]) #519
ConvNextT_raw_hare<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "hare", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hare_ConvNextT_act<-as.data.frame(ConvNextT_raw_hare@act)

hare_gt_act<-as.data.frame(t(hare_gt_act))
hare_gt_act$n<-hare_length_gt_raw
hare_ResNet18_act<-as.data.frame(t(hare_ResNet18_act))
hare_ResNet18_act$n<-hare_length_ResNet18_raw
hare_ResNet50_act<-as.data.frame(t(hare_ResNet50_act))
hare_ResNet50_act$n<-hare_length_ResNet50_raw
hare_ConvNextT_act<-as.data.frame(t(hare_ConvNextT_act))
hare_ConvNextT_act$n<-hare_length_ConvNextT_raw
##hartebeest_cokes #93 GT records- done####

##GT
hartebeest_cokes_length_gt_raw<-length(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_hartebeest_cokes<- fitact(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_hartebeest_cokes, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "hartebeest_cokes", cex.main=1)
hartebeest_cokes_gt_act<-as.data.frame(gt_raw_hartebeest_cokes@act) # view estimates of proportion of time active

#ResNet18 raw
hartebeest_cokes_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "hartebeest_cokes", "time_rad"])
ResNet18_raw_hartebeest_cokes<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "hartebeest_cokes", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hartebeest_cokes_ResNet18_act<-as.data.frame(ResNet18_raw_hartebeest_cokes@act)

#ResNet50 raw
hartebeest_cokes_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "hartebeest_cokes", "time_rad"])
ResNet50_raw_hartebeest_cokes<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "hartebeest_cokes", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hartebeest_cokes_ResNet50_act<-as.data.frame(ResNet50_raw_hartebeest_cokes@act)

#ConvNextT 90%
hartebeest_cokes_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "hartebeest_cokes", "time_rad"]) #519
ConvNextT_raw_hartebeest_cokes<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "hartebeest_cokes", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hartebeest_cokes_ConvNextT_act<-as.data.frame(ConvNextT_raw_hartebeest_cokes@act)

hartebeest_cokes_gt_act<-as.data.frame(t(hartebeest_cokes_gt_act))
hartebeest_cokes_gt_act$n<-hartebeest_cokes_length_gt_raw
hartebeest_cokes_ResNet18_act<-as.data.frame(t(hartebeest_cokes_ResNet18_act))
hartebeest_cokes_ResNet18_act$n<-hartebeest_cokes_length_ResNet18_raw
hartebeest_cokes_ResNet50_act<-as.data.frame(t(hartebeest_cokes_ResNet50_act))
hartebeest_cokes_ResNet50_act$n<-hartebeest_cokes_length_ResNet50_raw
hartebeest_cokes_ConvNextT_act<-as.data.frame(t(hartebeest_cokes_ConvNextT_act))
hartebeest_cokes_ConvNextT_act$n<-hartebeest_cokes_length_ConvNextT_raw
##hippopotamus #92 GT records- done####

##GT
hippopotamus_length_gt_raw<-length(gt_raw[gt_raw$label == "hippopotamus", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_hippopotamus<- fitact(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_hippopotamus, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "hippopotamus", cex.main=1)
hippopotamus_gt_act<-as.data.frame(gt_raw_hippopotamus@act) # view estimates of proportion of time active

#ResNet18 raw
hippopotamus_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "hippopotamus", "time_rad"])
ResNet18_raw_hippopotamus<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "hippopotamus", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hippopotamus_ResNet18_act<-as.data.frame(ResNet18_raw_hippopotamus@act)

#ResNet50 raw
hippopotamus_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "hippopotamus", "time_rad"])
ResNet50_raw_hippopotamus<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "hippopotamus", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hippopotamus_ResNet50_act<-as.data.frame(ResNet50_raw_hippopotamus@act)

#ConvNextT 90%
hippopotamus_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "hippopotamus", "time_rad"]) #519
ConvNextT_raw_hippopotamus<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "hippopotamus", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hippopotamus_ConvNextT_act<-as.data.frame(ConvNextT_raw_hippopotamus@act)

hippopotamus_gt_act<-as.data.frame(t(hippopotamus_gt_act))
hippopotamus_gt_act$n<-hippopotamus_length_gt_raw
hippopotamus_ResNet18_act<-as.data.frame(t(hippopotamus_ResNet18_act))
hippopotamus_ResNet18_act$n<-hippopotamus_length_ResNet18_raw
hippopotamus_ResNet50_act<-as.data.frame(t(hippopotamus_ResNet50_act))
hippopotamus_ResNet50_act$n<-hippopotamus_length_ResNet50_raw
hippopotamus_ConvNextT_act<-as.data.frame(t(hippopotamus_ConvNextT_act))
hippopotamus_ConvNextT_act$n<-hippopotamus_length_ConvNextT_raw
##hyena_aardwolf #327 GT records - done####

##GT
hyena_aardwolf_length_gt_raw<-length(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_hyena_aardwolf<- fitact(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_hyena_aardwolf, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "hyena_aardwolf", cex.main=1)
hyena_aardwolf_gt_act<-as.data.frame(gt_raw_hyena_aardwolf@act) # view estimates of proportion of time active

#ResNet18 raw
hyena_aardwolf_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "hyena_aardwolf", "time_rad"])
ResNet18_raw_hyena_aardwolf<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "hyena_aardwolf", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hyena_aardwolf_ResNet18_act<-as.data.frame(ResNet18_raw_hyena_aardwolf@act)

#ResNet50 raw
hyena_aardwolf_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "hyena_aardwolf", "time_rad"])
ResNet50_raw_hyena_aardwolf<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "hyena_aardwolf", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hyena_aardwolf_ResNet50_act<-as.data.frame(ResNet50_raw_hyena_aardwolf@act)

#ConvNextT 90%
hyena_aardwolf_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "hyena_aardwolf", "time_rad"]) #519
ConvNextT_raw_hyena_aardwolf<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "hyena_aardwolf", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
hyena_aardwolf_ConvNextT_act<-as.data.frame(ConvNextT_raw_hyena_aardwolf@act)

hyena_aardwolf_gt_act<-as.data.frame(t(hyena_aardwolf_gt_act))
hyena_aardwolf_gt_act$n<-hyena_aardwolf_length_gt_raw
hyena_aardwolf_ResNet18_act<-as.data.frame(t(hyena_aardwolf_ResNet18_act))
hyena_aardwolf_ResNet18_act$n<-hyena_aardwolf_length_ResNet18_raw
hyena_aardwolf_ResNet50_act<-as.data.frame(t(hyena_aardwolf_ResNet50_act))
hyena_aardwolf_ResNet50_act$n<-hyena_aardwolf_length_ResNet50_raw
hyena_aardwolf_ConvNextT_act<-as.data.frame(t(hyena_aardwolf_ConvNextT_act))
hyena_aardwolf_ConvNextT_act$n<-hyena_aardwolf_length_ConvNextT_raw
##impala #1924 GT records- done####

##GT
impala_length_gt_raw<-length(gt_raw[gt_raw$label == "impala", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_impala<- fitact(gt_raw[gt_raw$label == "impala", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_impala, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "impala", cex.main=1)
impala_gt_act<-as.data.frame(gt_raw_impala@act) # view estimates of proportion of time active

#ResNet18 raw
impala_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "impala", "time_rad"])
ResNet18_raw_impala<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "impala", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
impala_ResNet18_act<-as.data.frame(ResNet18_raw_impala@act)

#ResNet50 raw
impala_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "impala", "time_rad"])
ResNet50_raw_impala<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "impala", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
impala_ResNet50_act<-as.data.frame(ResNet50_raw_impala@act)

#ConvNextT 90%
impala_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "impala", "time_rad"]) #519
ConvNextT_raw_impala<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "impala", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
impala_ConvNextT_act<-as.data.frame(ConvNextT_raw_impala@act)

impala_gt_act<-as.data.frame(t(impala_gt_act))
impala_gt_act$n<-impala_length_gt_raw
impala_ResNet18_act<-as.data.frame(t(impala_ResNet18_act))
impala_ResNet18_act$n<-impala_length_ResNet18_raw
impala_ResNet50_act<-as.data.frame(t(impala_ResNet50_act))
impala_ResNet50_act$n<-impala_length_ResNet50_raw
impala_ConvNextT_act<-as.data.frame(t(impala_ConvNextT_act))
impala_ConvNextT_act$n<-impala_length_ConvNextT_raw
##jackal #114 GT records- done####

##GT
jackal_length_gt_raw<-length(gt_raw[gt_raw$label == "jackal", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_jackal<- fitact(gt_raw[gt_raw$label == "jackal", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_jackal, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "jackal", cex.main=1)
jackal_gt_act<-as.data.frame(gt_raw_jackal@act) # view estimates of proportion of time active

#ResNet18 raw
jackal_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "jackal", "time_rad"])
ResNet18_raw_jackal<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "jackal", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
jackal_ResNet18_act<-as.data.frame(ResNet18_raw_jackal@act)

#ResNet50 raw
jackal_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "jackal", "time_rad"])
ResNet50_raw_jackal<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "jackal", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
jackal_ResNet50_act<-as.data.frame(ResNet50_raw_jackal@act)

#ConvNextT 90%
jackal_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "jackal", "time_rad"]) #519
ConvNextT_raw_jackal<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "jackal", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
jackal_ConvNextT_act<-as.data.frame(ConvNextT_raw_jackal@act)

jackal_gt_act<-as.data.frame(t(jackal_gt_act))
jackal_gt_act$n<-jackal_length_gt_raw
jackal_ResNet18_act<-as.data.frame(t(jackal_ResNet18_act))
jackal_ResNet18_act$n<-jackal_length_ResNet18_raw
jackal_ResNet50_act<-as.data.frame(t(jackal_ResNet50_act))
jackal_ResNet50_act$n<-jackal_length_ResNet50_raw
jackal_ConvNextT_act<-as.data.frame(t(jackal_ConvNextT_act))
jackal_ConvNextT_act$n<-jackal_length_ConvNextT_raw
##leopard #1 record- dont run####

##GT
leopard_length_gt_raw<-length(gt_raw[gt_raw$label == "leopard", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_leopard<- fitact(gt_raw[gt_raw$label == "leopard", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_leopard, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "leopard", cex.main=1)
leopard_gt_act<-as.data.frame(gt_raw_leopard@act) # view estimates of proportion of time active

#ResNet18 raw
leopard_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "leopard", "time_rad"])
ResNet18_raw_leopard<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "leopard", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
leopard_ResNet18_act<-as.data.frame(ResNet18_raw_leopard@act)

#ResNet50 raw
leopard_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "leopard", "time_rad"])
ResNet50_raw_leopard<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "leopard", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
leopard_ResNet50_act<-as.data.frame(ResNet50_raw_leopard@act)

#ConvNextT 90%
leopard_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "leopard", "time_rad"]) #519
ConvNextT_raw_leopard<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "leopard", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
leopard_ConvNextT_act<-as.data.frame(ConvNextT_raw_leopard@act)

leopard_gt_act<-as.data.frame(t(leopard_gt_act))
leopard_gt_act$n<-leopard_length_gt_raw
leopard_ResNet18_act<-as.data.frame(t(leopard_ResNet18_act))
leopard_ResNet18_act$n<-leopard_length_ResNet18_raw
leopard_ResNet50_act<-as.data.frame(t(leopard_ResNet50_act))
leopard_ResNet50_act$n<-leopard_length_ResNet50_raw
leopard_ConvNextT_act<-as.data.frame(t(leopard_ConvNextT_act))
leopard_ConvNextT_act$n<-leopard_length_ConvNextT_raw
##lion #16 gt records- dont run####

##GT
lion_length_gt_raw<-length(gt_raw[gt_raw$label == "lion", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_lion<- fitact(gt_raw[gt_raw$label == "lion", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_lion, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "lion", cex.main=1)
lion_gt_act<-as.data.frame(gt_raw_lion@act) # view estimates of proportion of time active

#ResNet18 raw
lion_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "lion", "time_rad"])
ResNet18_raw_lion<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "lion", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
lion_ResNet18_act<-as.data.frame(ResNet18_raw_lion@act)

#ResNet50 raw
lion_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "lion", "time_rad"])
ResNet50_raw_lion<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "lion", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
lion_ResNet50_act<-as.data.frame(ResNet50_raw_lion@act)

#ConvNextT 90%
lion_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "lion", "time_rad"]) #519
ConvNextT_raw_lion<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "lion", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
lion_ConvNextT_act<-as.data.frame(ConvNextT_raw_lion@act)

lion_gt_act<-as.data.frame(t(lion_gt_act))
lion_gt_act$n<-lion_length_gt_raw
lion_ResNet18_act<-as.data.frame(t(lion_ResNet18_act))
lion_ResNet18_act$n<-lion_length_ResNet18_raw
lion_ResNet50_act<-as.data.frame(t(lion_ResNet50_act))
lion_ResNet50_act$n<-lion_length_ResNet50_raw
lion_ConvNextT_act<-as.data.frame(t(lion_ConvNextT_act))
lion_ConvNextT_act$n<-lion_length_ConvNextT_raw
##mongoose #143 gt records - done####

##GT
mongoose_length_gt_raw<-length(gt_raw[gt_raw$label == "mongoose", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_mongoose<- fitact(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_mongoose, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "mongoose", cex.main=1)
mongoose_gt_act<-as.data.frame(gt_raw_mongoose@act) # view estimates of proportion of time active

#ResNet18 raw
mongoose_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "mongoose", "time_rad"])
ResNet18_raw_mongoose<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "mongoose", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
mongoose_ResNet18_act<-as.data.frame(ResNet18_raw_mongoose@act)

#ResNet50 raw
mongoose_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "mongoose", "time_rad"])
ResNet50_raw_mongoose<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "mongoose", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
mongoose_ResNet50_act<-as.data.frame(ResNet50_raw_mongoose@act)

#ConvNextT 90%
mongoose_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "mongoose", "time_rad"]) #519
ConvNextT_raw_mongoose<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "mongoose", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
mongoose_ConvNextT_act<-as.data.frame(ConvNextT_raw_mongoose@act)

mongoose_gt_act<-as.data.frame(t(mongoose_gt_act))
mongoose_gt_act$n<-mongoose_length_gt_raw
mongoose_ResNet18_act<-as.data.frame(t(mongoose_ResNet18_act))
mongoose_ResNet18_act$n<-mongoose_length_ResNet18_raw
mongoose_ResNet50_act<-as.data.frame(t(mongoose_ResNet50_act))
mongoose_ResNet50_act$n<-mongoose_length_ResNet50_raw
mongoose_ConvNextT_act<-as.data.frame(t(mongoose_ConvNextT_act))
mongoose_ConvNextT_act$n<-mongoose_length_ConvNextT_raw
##oribi #5 gt records- dont run####

##GT
oribi_length_gt_raw<-length(gt_raw[gt_raw$label == "oribi", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_oribi<- fitact(gt_raw[gt_raw$label == "oribi", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_oribi, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "oribi", cex.main=1)
oribi_gt_act<-as.data.frame(gt_raw_oribi@act) # view estimates of proportion of time active

#ResNet18 raw
oribi_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "oribi", "time_rad"])
ResNet18_raw_oribi<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "oribi", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
oribi_ResNet18_act<-as.data.frame(ResNet18_raw_oribi@act)

#ResNet50 raw
oribi_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "oribi", "time_rad"])
ResNet50_raw_oribi<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "oribi", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
oribi_ResNet50_act<-as.data.frame(ResNet50_raw_oribi@act)

#ConvNextT 90%
oribi_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "oribi", "time_rad"]) #519
ConvNextT_raw_oribi<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "oribi", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
oribi_ConvNextT_act<-as.data.frame(ConvNextT_raw_oribi@act)

oribi_gt_act<-as.data.frame(t(oribi_gt_act))
oribi_gt_act$n<-oribi_length_gt_raw
oribi_ResNet18_act<-as.data.frame(t(oribi_ResNet18_act))
oribi_ResNet18_act$n<-oribi_length_ResNet18_raw
oribi_ResNet50_act<-as.data.frame(t(oribi_ResNet50_act))
oribi_ResNet50_act$n<-oribi_length_ResNet50_raw
oribi_ConvNextT_act<-as.data.frame(t(oribi_ConvNextT_act))
oribi_ConvNextT_act$n<-oribi_length_ConvNextT_raw
##porcupine #8 gt records- dont run####

##GT
porcupine_length_gt_raw<-length(gt_raw[gt_raw$label == "porcupine", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_porcupine<- fitact(gt_raw[gt_raw$label == "porcupine", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_porcupine, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "porcupine", cex.main=1)
porcupine_gt_act<-as.data.frame(gt_raw_porcupine@act) # view estimates of proportion of time active

#ResNet18 raw
porcupine_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "porcupine", "time_rad"])
ResNet18_raw_porcupine<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "porcupine", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
porcupine_ResNet18_act<-as.data.frame(ResNet18_raw_porcupine@act)

#ResNet50 raw
porcupine_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "porcupine", "time_rad"])
ResNet50_raw_porcupine<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "porcupine", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
porcupine_ResNet50_act<-as.data.frame(ResNet50_raw_porcupine@act)

#ConvNextT 90%
porcupine_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "porcupine", "time_rad"]) #519
ConvNextT_raw_porcupine<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "porcupine", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
porcupine_ConvNextT_act<-as.data.frame(ConvNextT_raw_porcupine@act)

porcupine_gt_act<-as.data.frame(t(porcupine_gt_act))
porcupine_gt_act$n<-porcupine_length_gt_raw
porcupine_ResNet18_act<-as.data.frame(t(porcupine_ResNet18_act))
porcupine_ResNet18_act$n<-porcupine_length_ResNet18_raw
porcupine_ResNet50_act<-as.data.frame(t(porcupine_ResNet50_act))
porcupine_ResNet50_act$n<-porcupine_length_ResNet50_raw
porcupine_ConvNextT_act<-as.data.frame(t(porcupine_ConvNextT_act))
porcupine_ConvNextT_act$n<-porcupine_length_ConvNextT_raw
##reedbuck #1 gt record- dont run####

##GT
reedbuck_length_gt_raw<-length(gt_raw[gt_raw$label == "reedbuck", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_reedbuck<- fitact(gt_raw[gt_raw$label == "reedbuck", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_reedbuck, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "reedbuck", cex.main=1)
reedbuck_gt_act<-as.data.frame(gt_raw_reedbuck@act) # view estimates of proportion of time active

#ResNet18 raw
reedbuck_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "reedbuck", "time_rad"])
ResNet18_raw_reedbuck<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "reedbuck", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
reedbuck_ResNet18_act<-as.data.frame(ResNet18_raw_reedbuck@act)

#ResNet50 raw
reedbuck_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "reedbuck", "time_rad"])
ResNet50_raw_reedbuck<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "reedbuck", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
reedbuck_ResNet50_act<-as.data.frame(ResNet50_raw_reedbuck@act)

#ConvNextT 90%
reedbuck_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "reedbuck", "time_rad"]) #519
ConvNextT_raw_reedbuck<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "reedbuck", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
reedbuck_ConvNextT_act<-as.data.frame(ConvNextT_raw_reedbuck@act)

reedbuck_gt_act<-as.data.frame(t(reedbuck_gt_act))
reedbuck_gt_act$n<-reedbuck_length_gt_raw
reedbuck_ResNet18_act<-as.data.frame(t(reedbuck_ResNet18_act))
reedbuck_ResNet18_act$n<-reedbuck_length_ResNet18_raw
reedbuck_ResNet50_act<-as.data.frame(t(reedbuck_ResNet50_act))
reedbuck_ResNet50_act$n<-reedbuck_length_ResNet50_raw
reedbuck_ConvNextT_act<-as.data.frame(t(reedbuck_ConvNextT_act))
reedbuck_ConvNextT_act$n<-reedbuck_length_ConvNextT_raw
##serval #7 gt records- dont run####

##GT
serval_length_gt_raw<-length(gt_raw[gt_raw$label == "serval", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_serval<- fitact(gt_raw[gt_raw$label == "serval", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_serval, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "serval", cex.main=1)
serval_gt_act<-as.data.frame(gt_raw_serval@act) # view estimates of proportion of time active

#ResNet18 raw
serval_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "serval", "time_rad"])
ResNet18_raw_serval<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "serval", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
serval_ResNet18_act<-as.data.frame(ResNet18_raw_serval@act)

#ResNet50 raw
serval_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "serval", "time_rad"])
ResNet50_raw_serval<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "serval", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
serval_ResNet50_act<-as.data.frame(ResNet50_raw_serval@act)

#ConvNextT 90%
serval_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "serval", "time_rad"]) #519
ConvNextT_raw_serval<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "serval", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
serval_ConvNextT_act<-as.data.frame(ConvNextT_raw_serval@act)

serval_gt_act<-as.data.frame(t(serval_gt_act))
serval_gt_act$n<-serval_length_gt_raw
serval_ResNet18_act<-as.data.frame(t(serval_ResNet18_act))
serval_ResNet18_act$n<-serval_length_ResNet18_raw
serval_ResNet50_act<-as.data.frame(t(serval_ResNet50_act))
serval_ResNet50_act$n<-serval_length_ResNet50_raw
serval_ConvNextT_act<-as.data.frame(t(serval_ConvNextT_act))
serval_ConvNextT_act$n<-serval_length_ConvNextT_raw
##topi #810 GT records- done####

##GT
topi_length_gt_raw<-length(gt_raw[gt_raw$label == "topi", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_topi<- fitact(gt_raw[gt_raw$label == "topi", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_topi, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "topi", cex.main=1)
topi_gt_act<-as.data.frame(gt_raw_topi@act) # view estimates of proportion of time active

#ResNet18 raw
topi_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "topi", "time_rad"])
ResNet18_raw_topi<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "topi", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
topi_ResNet18_act<-as.data.frame(ResNet18_raw_topi@act)

#ResNet50 raw
topi_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "topi", "time_rad"])
ResNet50_raw_topi<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "topi", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
topi_ResNet50_act<-as.data.frame(ResNet50_raw_topi@act)

#ConvNextT 90%
topi_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "topi", "time_rad"]) #519
ConvNextT_raw_topi<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "topi", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
topi_ConvNextT_act<-as.data.frame(ConvNextT_raw_topi@act)

topi_gt_act<-as.data.frame(t(topi_gt_act))
topi_gt_act$n<-topi_length_gt_raw
topi_ResNet18_act<-as.data.frame(t(topi_ResNet18_act))
topi_ResNet18_act$n<-topi_length_ResNet18_raw
topi_ResNet50_act<-as.data.frame(t(topi_ResNet50_act))
topi_ResNet50_act$n<-topi_length_ResNet50_raw
topi_ConvNextT_act<-as.data.frame(t(topi_ConvNextT_act))
topi_ConvNextT_act$n<-topi_length_ConvNextT_raw
##vervet_monkey #32 gt records - done####

##GT
vervet_monkey_length_gt_raw<-length(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_vervet_monkey<- fitact(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_vervet_monkey, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "vervet_monkey", cex.main=1)
vervet_monkey_gt_act<-as.data.frame(gt_raw_vervet_monkey@act) # view estimates of proportion of time active

#ResNet18 raw
vervet_monkey_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "vervet_monkey", "time_rad"])
ResNet18_raw_vervet_monkey<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "vervet_monkey", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
vervet_monkey_ResNet18_act<-as.data.frame(ResNet18_raw_vervet_monkey@act)

#ResNet50 raw
vervet_monkey_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "vervet_monkey", "time_rad"])
ResNet50_raw_vervet_monkey<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "vervet_monkey", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
vervet_monkey_ResNet50_act<-as.data.frame(ResNet50_raw_vervet_monkey@act)

#ConvNextT 90%
vervet_monkey_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "vervet_monkey", "time_rad"]) #519
ConvNextT_raw_vervet_monkey<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "vervet_monkey", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
vervet_monkey_ConvNextT_act<-as.data.frame(ConvNextT_raw_vervet_monkey@act)

vervet_monkey_gt_act<-as.data.frame(t(vervet_monkey_gt_act))
vervet_monkey_gt_act$n<-vervet_monkey_length_gt_raw
vervet_monkey_ResNet18_act<-as.data.frame(t(vervet_monkey_ResNet18_act))
vervet_monkey_ResNet18_act$n<-vervet_monkey_length_ResNet18_raw
vervet_monkey_ResNet50_act<-as.data.frame(t(vervet_monkey_ResNet50_act))
vervet_monkey_ResNet50_act$n<-vervet_monkey_length_ResNet50_raw
vervet_monkey_ConvNextT_act<-as.data.frame(t(vervet_monkey_ConvNextT_act))
vervet_monkey_ConvNextT_act$n<-vervet_monkey_length_ConvNextT_raw
##warthog #523 gt records- done####

##GT
warthog_length_gt_raw<-length(gt_raw[gt_raw$label == "warthog", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_warthog<- fitact(gt_raw[gt_raw$label == "warthog", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_warthog, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "warthog", cex.main=1)
warthog_gt_act<-as.data.frame(gt_raw_warthog@act) # view estimates of proportion of time active

#ResNet18 raw
warthog_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "warthog", "time_rad"])
ResNet18_raw_warthog<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "warthog", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
warthog_ResNet18_act<-as.data.frame(ResNet18_raw_warthog@act)

#ResNet50 raw
warthog_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "warthog", "time_rad"])
ResNet50_raw_warthog<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "warthog", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
warthog_ResNet50_act<-as.data.frame(ResNet50_raw_warthog@act)

#ConvNextT 90%
warthog_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "warthog", "time_rad"]) #519
ConvNextT_raw_warthog<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "warthog", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
warthog_ConvNextT_act<-as.data.frame(ConvNextT_raw_warthog@act)

warthog_gt_act<-as.data.frame(t(warthog_gt_act))
warthog_gt_act$n<-warthog_length_gt_raw
warthog_ResNet18_act<-as.data.frame(t(warthog_ResNet18_act))
warthog_ResNet18_act$n<-warthog_length_ResNet18_raw
warthog_ResNet50_act<-as.data.frame(t(warthog_ResNet50_act))
warthog_ResNet50_act$n<-warthog_length_ResNet50_raw
warthog_ConvNextT_act<-as.data.frame(t(warthog_ConvNextT_act))
warthog_ConvNextT_act$n<-warthog_length_ConvNextT_raw
##waterbuck #3 gt records- dont run####

##GT
waterbuck_length_gt_raw<-length(gt_raw[gt_raw$label == "waterbuck", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_waterbuck<- fitact(gt_raw[gt_raw$label == "waterbuck", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_waterbuck, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "waterbuck", cex.main=1)
waterbuck_gt_act<-as.data.frame(gt_raw_waterbuck@act) # view estimates of proportion of time active

#ResNet18 raw
waterbuck_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "waterbuck", "time_rad"])
ResNet18_raw_waterbuck<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "waterbuck", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
waterbuck_ResNet18_act<-as.data.frame(ResNet18_raw_waterbuck@act)

#ResNet50 raw
waterbuck_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "waterbuck", "time_rad"])
ResNet50_raw_waterbuck<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "waterbuck", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
waterbuck_ResNet50_act<-as.data.frame(ResNet50_raw_waterbuck@act)

#ConvNextT 90%
waterbuck_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "waterbuck", "time_rad"]) #519
ConvNextT_raw_waterbuck<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "waterbuck", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
waterbuck_ConvNextT_act<-as.data.frame(ConvNextT_raw_waterbuck@act)

waterbuck_gt_act<-as.data.frame(t(waterbuck_gt_act))
waterbuck_gt_act$n<-waterbuck_length_gt_raw
waterbuck_ResNet18_act<-as.data.frame(t(waterbuck_ResNet18_act))
waterbuck_ResNet18_act$n<-waterbuck_length_ResNet18_raw
waterbuck_ResNet50_act<-as.data.frame(t(waterbuck_ResNet50_act))
waterbuck_ResNet50_act$n<-waterbuck_length_ResNet50_raw
waterbuck_ConvNextT_act<-as.data.frame(t(waterbuck_ConvNextT_act))
waterbuck_ConvNextT_act$n<-waterbuck_length_ConvNextT_raw
##wildebeest #5460 GT records - done####

##GT
wildebeest_length_gt_raw<-length(gt_raw[gt_raw$label == "wildebeest", "time_rad"]) #519

# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_wildebeest<- fitact(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                           reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_wildebeest, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "wildebeest", cex.main=1)

wildebeest_gt_act<-as.data.frame(gt_raw_wildebeest@act) # view estimates of proportion of time active

#ResNet18 raw
wildebeest_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "wildebeest", "time_rad"])
ResNet18_raw_wildebeest<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "wildebeest", "time_rad"],
                                 reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
wildebeest_ResNet18_act<-as.data.frame(ResNet18_raw_wildebeest@act)

#ResNet50 raw
wildebeest_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "wildebeest", "time_rad"])
ResNet50_raw_wildebeest<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "wildebeest", "time_rad"],
                                 reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
wildebeest_ResNet50_act<-as.data.frame(ResNet50_raw_wildebeest@act)

#ConvNextT 90%
wildebeest_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "wildebeest", "time_rad"]) #519
ConvNextT_raw_wildebeest<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "wildebeest", "time_rad"],
                                  reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
wildebeest_ConvNextT_act<-as.data.frame(ConvNextT_raw_wildebeest@act)

wildebeest_gt_act<-as.data.frame(t(wildebeest_gt_act))
wildebeest_gt_act$n<-wildebeest_length_gt_raw
wildebeest_ResNet18_act<-as.data.frame(t(wildebeest_ResNet18_act))
wildebeest_ResNet18_act$n<-wildebeest_length_ResNet18_raw
wildebeest_ResNet50_act<-as.data.frame(t(wildebeest_ResNet50_act))
wildebeest_ResNet50_act$n<-wildebeest_length_ResNet50_raw
wildebeest_ConvNextT_act<-as.data.frame(t(wildebeest_ConvNextT_act))
wildebeest_ConvNextT_act$n<-wildebeest_length_ConvNextT_raw

##zebra #3400 GT records- done####

##GT
zebra_length_gt_raw<-length(gt_raw[gt_raw$label == "zebra", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_zebra<- fitact(gt_raw[gt_raw$label == "zebra", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_zebra, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "zebra", cex.main=1)
zebra_gt_act<-as.data.frame(gt_raw_zebra@act) # view estimates of proportion of time active

#ResNet18 raw
zebra_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "zebra", "time_rad"])
ResNet18_raw_zebra<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "zebra", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
zebra_ResNet18_act<-as.data.frame(ResNet18_raw_zebra@act)

#ResNet50 raw
zebra_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "zebra", "time_rad"])
ResNet50_raw_zebra<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "zebra", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
zebra_ResNet50_act<-as.data.frame(ResNet50_raw_zebra@act)

#ConvNextT 90%
zebra_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "zebra", "time_rad"]) #519
ConvNextT_raw_zebra<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "zebra", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
zebra_ConvNextT_act<-as.data.frame(ConvNextT_raw_zebra@act)

zebra_gt_act<-as.data.frame(t(zebra_gt_act))
zebra_gt_act$n<-zebra_length_gt_raw
zebra_ResNet18_act<-as.data.frame(t(zebra_ResNet18_act))
zebra_ResNet18_act$n<-zebra_length_ResNet18_raw
zebra_ResNet50_act<-as.data.frame(t(zebra_ResNet50_act))
zebra_ResNet50_act$n<-zebra_length_ResNet50_raw
zebra_ConvNextT_act<-as.data.frame(t(zebra_ConvNextT_act))
zebra_ConvNextT_act$n<-zebra_length_ConvNextT_raw
##zorilla #4 gt records- dont run####

##GT
zorilla_length_gt_raw<-length(gt_raw[gt_raw$label == "zorilla", "time_rad"]) #519
# estimating proportion of time active for wild boar for the whole dataset (overall)  
gt_raw_zorilla<- fitact(gt_raw[gt_raw$label == "zorilla", "time_rad"],
                         reps=1000,sample="model",show=TRUE); beep()
#plot(gt_raw_zorilla, centre= "day",  tline=list(lty=1,lwd=2, col="red"), 
# cline=list(lty=2,lwd=1.5, col="black"), dline=list(col="white"),main= "zorilla", cex.main=1)
zorilla_gt_act<-as.data.frame(gt_raw_zorilla@act) # view estimates of proportion of time active

#ResNet18 raw
zorilla_length_ResNet18_raw<-length(ResNet18_raw[ResNet18_raw$pred1_label == "zorilla", "time_rad"])
ResNet18_raw_zorilla<- fitact(ResNet18_raw[ResNet18_raw$pred1_label == "zorilla", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
zorilla_ResNet18_act<-as.data.frame(ResNet18_raw_zorilla@act)

#ResNet50 raw
zorilla_length_ResNet50_raw<-length(ResNet50_raw[ResNet50_raw$pred1_label == "zorilla", "time_rad"])
ResNet50_raw_zorilla<- fitact(ResNet50_raw[ResNet50_raw$pred1_label == "zorilla", "time_rad"],
                               reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
zorilla_ResNet50_act<-as.data.frame(ResNet50_raw_zorilla@act)

#ConvNextT 90%
zorilla_length_ConvNextT_raw<-length(ConvNextT_raw[ConvNextT_raw$pred1_label == "zorilla", "time_rad"]) #519
ConvNextT_raw_zorilla<- fitact(ConvNextT_raw[ConvNextT_raw$pred1_label == "zorilla", "time_rad"],
                                reps=1000,sample="model",show=TRUE); beep()
# view estimates of proportion of time active
zorilla_ConvNextT_act<-as.data.frame(ConvNextT_raw_zorilla@act)

zorilla_gt_act<-as.data.frame(t(zorilla_gt_act))
zorilla_gt_act$n<-zorilla_length_gt_raw
zorilla_ResNet18_act<-as.data.frame(t(zorilla_ResNet18_act))
zorilla_ResNet18_act$n<-zorilla_length_ResNet18_raw
zorilla_ResNet50_act<-as.data.frame(t(zorilla_ResNet50_act))
zorilla_ResNet50_act$n<-zorilla_length_ResNet50_raw
zorilla_ConvNextT_act<-as.data.frame(t(zorilla_ConvNextT_act))
zorilla_ConvNextT_act$n<-zorilla_length_ConvNextT_raw
#combine and write ####


#aardvark_act<-rbind(aardvark_gt_act, aardvark_ResNet18_act, aardvark_ResNet50_act, aardvark_ConvNextT_act)
baboon_act<-rbind(baboon_gt_act, baboon_ResNet18_act, baboon_ResNet50_act, baboon_ConvNextT_act)
#bateared_fox_act<-rbind(bateared_fox_gt_act, bateared_fox_ResNet18_act, bateared_fox_ResNet50_act, bateared_fox_ConvNextT_act)
buffalo_act<-rbind(buffalo_gt_act, buffalo_ResNet18_act, buffalo_ResNet50_act, buffalo_ConvNextT_act)
#bushbuck_act<-rbind(bushbuck_gt_act, bushbuck_ResNet18_act, bushbuck_ResNet50_act, bushbuck_ConvNextT_act)
#cheetah_act<-rbind(cheetah_gt_act, cheetah_ResNet18_act, cheetah_ResNet50_act, cheetah_ConvNextT_act)
dikdik_act<-rbind(dikdik_gt_act, dikdik_ResNet18_act, dikdik_ResNet50_act, dikdik_ConvNextT_act)
eland_act<-rbind(eland_gt_act, eland_ResNet18_act, eland_ResNet50_act, eland_ConvNextT_act)
elephant_act<-rbind(elephant_gt_act, elephant_ResNet18_act, elephant_ResNet50_act, elephant_ConvNextT_act)
gazelle_grants_act<-rbind(gazelle_grants_gt_act, gazelle_grants_ResNet18_act, gazelle_grants_ResNet50_act, gazelle_grants_ConvNextT_act)
gazelle_thomsons_act<-rbind(gazelle_thomsons_gt_act, gazelle_thomsons_ResNet18_act, gazelle_thomsons_ResNet50_act, gazelle_thomsons_ConvNextT_act)
#genet_act<-rbind(genet_gt_act, genet_ResNet18_act, genet_ResNet50_act, genet_ConvNextT_act)
giraffe_act<-rbind(giraffe_gt_act, giraffe_ResNet18_act, giraffe_ResNet50_act, giraffe_ConvNextT_act)
hare_act<-rbind(hare_gt_act, hare_ResNet18_act, hare_ResNet50_act, hare_ConvNextT_act)
hartebeest_cokes_act<-rbind(hartebeest_cokes_gt_act, hartebeest_cokes_ResNet18_act, hartebeest_cokes_ResNet50_act, hartebeest_cokes_ConvNextT_act)
hippopotamus_act<-rbind(hippopotamus_gt_act,hippopotamus_ResNet18_act, hippopotamus_ResNet50_act, hippopotamus_ConvNextT_act)
hyena_aardwolf_act<-rbind(hyena_aardwolf_gt_act,hyena_aardwolf_ResNet18_act, hyena_aardwolf_ResNet50_act, hyena_aardwolf_ConvNextT_act)
impala_act<-rbind(impala_gt_act,impala_ResNet18_act, impala_ResNet50_act, impala_ConvNextT_act)
jackal_act<-rbind(jackal_gt_act,jackal_ResNet18_act, jackal_ResNet50_act, jackal_ConvNextT_act)
#leopard_act<-rbind(leopard_gt_act,leopard_ResNet18_act, leopard_ResNet50_act, leopard_ConvNextT_act)
#lion_act<-rbind(lion_gt_act,lion_ResNet18_act, lion_ResNet50_act, lion_ConvNextT_act)
mongoose_act<-rbind(mongoose_gt_act,mongoose_ResNet18_act, mongoose_ResNet50_act, mongoose_ConvNextT_act)
#oribi_act<-rbind(oribi_gt_act,oribi_ResNet18_act, oribi_ResNet50_act, oribi_ConvNextT_act)
#porcupine_act<-rbind(porcupine_gt_act,porcupine_ResNet18_act, porcupine_ResNet50_act, porcupine_ConvNextT_act)
#reedbuck_act<-rbind(reedbuck_gt_act,reedbuck_ResNet18_act, reedbuck_ResNet50_act)
#serval_act<-rbind(serval_gt_act,serval_ResNet18_act, serval_ResNet50_act, serval_ConvNextT_act)
topi_act<-rbind(topi_gt_act,topi_ResNet18_act, topi_ResNet50_act, topi_ConvNextT_act)
vervet_monkey_act<-rbind(vervet_monkey_gt_act,vervet_monkey_ResNet18_act, vervet_monkey_ResNet50_act, vervet_monkey_ConvNextT_act)
warthog_act<-rbind(warthog_gt_act,warthog_ResNet18_act, warthog_ResNet50_act, warthog_ConvNextT_act)
#waterbuck_act<-rbind(waterbuck_gt_act,waterbuck_ResNet18_act, waterbuck_ResNet50_act, waterbuck_ConvNextT_act)
wildebeest_act<-rbind(wildebeest_gt_act,wildebeest_ResNet18_act, wildebeest_ResNet50_act, wildebeest_ConvNextT_act)
zebra_act<-rbind(zebra_gt_act,zebra_ResNet18_act, zebra_ResNet50_act, zebra_ConvNextT_act)
#zorilla_act<-rbind(zorilla_gt_act,zorilla_ResNet18_act, zorilla_ResNet50_act, zorilla_ConvNextT_act)



activity_nothreshold<-rbind(baboon_act,buffalo_act, dikdik_act, eland_act,
                            elephant_act, gazelle_grants_act, gazelle_thomsons_act, giraffe_act, hare_act, hartebeest_cokes_act,
                            hippopotamus_act, hyena_aardwolf_act, impala_act,jackal_act, mongoose_act,
                            topi_act,vervet_monkey_act,warthog_act,wildebeest_act,zebra_act)

###Restructuring dataframe####
library(tibble)
activity_nothreshold<- tibble::rownames_to_column(activity_nothreshold, "species")

head(activity_nothreshold)
library(dplyr)
library(tidyr)
activity_nothreshold<-activity_nothreshold %>%
  separate(species, c("model", "species"), "_raw_")
activity_nothreshold<-activity_nothreshold %>%
  separate(species, c("species", "type"), "@")
activity_nothreshold$type<-NULL
write.csv(activity_nothreshold,"activity_nothreshold.csv")
#edited in excel to remove % bc i cba to figure out how to do it in r sorry
activity_nothreshold<-read.csv("activity_nothreshold.csv")

gt <- subset(activity_nothreshold, model == "gt")
gt$X<-NULL
gt <-gt%>% 
  rename(
    act_gt = act,
    se_gt=se,
    lcl.2.5_gt=lcl.2.5,
    ucl.97.5_gt=ucl.97.5,
    n_gt=n
  )


ResNet18 <- subset(activity_nothreshold, model == "ResNet18")
ResNet18$X<-NULL
ResNet18 <-ResNet18%>% 
  rename(
    act_ResNet18 = act,
    se_ResNet18=se,
    lcl.2.5_ResNet18=lcl.2.5,
    ucl.97.5_ResNet18=ucl.97.5,
    n_ResNet18=n
  )


ResNet50 <- subset(activity_nothreshold, model == "ResNet50")
ResNet50$X<-NULL
ResNet50 <-ResNet50%>% 
  rename(
    act_ResNet50 = act,
    se_ResNet50=se,
    lcl.2.5_ResNet50=lcl.2.5,
    ucl.97.5_ResNet50=ucl.97.5,
    n_ResNet50=n
  )

ConvNextT <- subset(activity_nothreshold, model == "ConvNextT")
ConvNextT$X<-NULL
ConvNextT <-ConvNextT%>% 
  rename(
    act_ConvNextT = act,
    se_ConvNextT=se,
    lcl.2.5_ConvNextT=lcl.2.5,
    ucl.97.5_ConvNextT=ucl.97.5,
    n_ConvNextT=n
  )

gtVResNet18<-cbind(gt, ResNet18)
gtVResNet18<-gtVResNet18[ -c(1,8,9) ]
gtV18v50<-cbind(gtVResNet18, ResNet50)
gtV18v50vCT<-cbind(gtV18v50, ConvNextT)
head(gtV18v50vCT)
write.csv(gtV18v50vCT, "all_model_act.csv")

### Basic scatter plot ####
activity_nothreshold_all<-read.csv("all_model_act.csv")
library(hrbrthemes)
library(ggpmisc)

# using default formula, label and methods
ResNet18_plot<-ggplot(data = activity_nothreshold_all, aes(x=act_gt, y=act_ResNet18)) +
  stat_poly_line(se=FALSE) +
  stat_poly_eq() +
  geom_point(aes(color=species))+
  ylim(0, 0.8)+ 
  labs(y = "Activity level estimate (ResNet18)", x = "Activity level estimate (manual)")
ResNet18_plot<-ResNet18_plot+geom_smooth(method=lm ,data = activity_nothreshold_all, aes(x=act_gt, y=act_gt), color = "black", linetype="longdash")
ResNet18_plot<-ResNet18_plot+ geom_label_repel(aes(label = species),size = 2.8)


ResNet50_plot<-ggplot(data = activity_nothreshold_all, aes(x=act_gt, y=act_ResNet50)) +
  stat_poly_line(se=FALSE) +
  stat_poly_eq() +
  geom_point(aes(color=species))+
  ylim(0, 0.8)+ 
  labs(y = "Activity level estimate (ResNet50)", x = "Activity level estimate (manual)")
ResNet50_plot<-ResNet50_plot+geom_smooth(method=lm ,data = activity_nothreshold_all, aes(x=act_gt, y=act_gt), color = "black", linetype="longdash")
ResNet50_plot<-ResNet50_plot+ geom_label_repel(aes(label = species),size = 2.8)


ConvNextT_plot<-ggplot(data = activity_nothreshold_all, aes(x=act_gt, y=act_ConvNextT)) +
  stat_poly_line(se=FALSE) +
  stat_poly_eq() +
  geom_point(aes(color=species))+
  #geom_text(aes(label=species),hjust=0, vjust=0)+
  ylim(0, 0.8)+
  labs(y = "Activity level estimate (ConvNextT)", x = "Activity level estimate (manual)")
ConvNextT_plot<-ConvNextT_plot+geom_smooth(method=lm ,data = activity_nothreshold_all, aes(x=act_gt, y=act_gt), color = "black", linetype="longdash")

### Species activity plots####
plot(gt_raw_zebra, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.1), las=1, lwd=2)
plot(ResNet18_raw_zebra, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_zebra, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_zebra, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18", "ResNet18","ConvNextT"), col=1:4, lty=1)
title("zebra-no threshold")

###Go to overlap script####