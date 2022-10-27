#####overlap estimates ####
library(overlap)

## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
#adjust=c(0.8, 1, 4)
#To account for sample size in the model, we chose to use the Δ1 overlap coefficient when sample sizes were less than 50 and Δ4 for samples greater than 50 https://www.nature.com/articles/s41598-018-22638-6. 
#overlap::overlapPlot(gt_raw_baboon,ResNet18_raw_baboon, extend='lightsteelblue4',rug=TRUE)
#legend('topright', c("Cattle present","Cattle absent"),lty=c(1,2),y.intersp=1,col=c("black","blue"),bty='white')
#abline(v=c(6,17+59/60),lty=3)
#(dhats_dfcpca<-overlapEst(mazrad_dfcp,mazrad_dfca))
#bsout_dfcpca<-bootEst(bsmazrad_dfcp,bsmazrad_dfca,adjust=c(0.8,NA,NA))
#dim(bsout_dfcpca)
#colMeans(bsout_dfcpca)
#bs_dfcpca<-as.vector(bsout_dfcpca[,1])
#bootCI(dhats_dfcpca[1],bs_dfcpca)['basic0',]
#watson.two.test(mazrad_dfcp,mazrad_dfca, alpha=0.05)
##Compare cattle present with cattle absent: savanna
#compareAct(list(mazmod_svcp,mazmod_svca))

#baboon ##77 gt records- need to rerun on different dhat #####
bs.baboon.gt<-resample(gt_raw[gt_raw$label == "baboon", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_baboon_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "baboon", "time_rad"],
                     ResNet18_raw[ResNet18_raw$pred1_label == "baboon", "time_rad"]))
bs.baboon.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "baboon", "time_rad"],10000)
bsbaboon_gt_ResNet18_raw<-bootEst(bs.baboon.gt,bs.baboon.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsbaboon_gt_ResNet18_raw.mean <-as.vector(colMeans(bsbaboon_gt_ResNet18_raw))
bsbaboon_gt_ResNet18_raw.mean <-bsbaboon_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsbaboon_gt_ResNet18_rawVec<-as.vector(bsbaboon_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsbaboon_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_baboon_gt_ResNet18_raw[2],bsbaboon_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
baboon_gt_ResNet18_raw.overlap <- cbind(bsbaboon_gt_ResNet18_raw.mean,bsbaboon_gt_ResNet18_raw.CI)

baboon_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "baboon", "time_rad"],
                ResNet18_raw[ResNet18_raw$pred1_label == "baboon", "time_rad"])
#Test Statistic: 0.0132 
#P-value > 0.10

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_baboon_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "baboon", "time_rad"],
                                             ResNet50_raw[ResNet50_raw$pred1_label == "baboon", "time_rad"]))

bs.baboon.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "baboon", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsbaboon_gt_ResNet50_raw<-bootEst(bs.baboon.gt,bs.baboon.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsbaboon_gt_ResNet50_raw.mean <-as.vector(colMeans(bsbaboon_gt_ResNet50_raw))
bsbaboon_gt_ResNet50_raw.mean <-bsbaboon_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsbaboon_gt_ResNet50_rawVec<-as.vector(bsbaboon_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsbaboon_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_baboon_gt_ResNet50_raw[2],bsbaboon_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
baboon_gt_ResNet50_raw.overlap <- cbind(bsbaboon_gt_ResNet50_raw.mean,bsbaboon_gt_ResNet50_raw.CI)

baboon_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "baboon", "time_rad"],
                ResNet50_raw[ResNet50_raw$pred1_label == "baboon", "time_rad"])
#Test Statistic: 0.0064 
#P-value > 0.10

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_baboon_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "baboon", "time_rad"],
                                             ConvNextT_raw[ConvNextT_raw$pred1_label == "baboon", "time_rad"]))

bs.baboon.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "baboon", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsbaboon_gt_ConvNextT_raw<-bootEst(bs.baboon.gt,bs.baboon.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsbaboon_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsbaboon_gt_ConvNextT_raw))
bsbaboon_gt_ConvNextT_raw.mean <-bsbaboon_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsbaboon_gt_ConvNextT_rawVec<-as.vector(bsbaboon_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsbaboon_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_baboon_gt_ConvNextT_raw[2],bsbaboon_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
baboon_gt_ConvNextT_raw.overlap <- cbind(bsbaboon_gt_ConvNextT_raw.mean,bsbaboon_gt_ConvNextT_raw.CI)
#
baboon_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "baboon", "time_rad"],
                ConvNextT_raw[ConvNextT_raw$pred1_label == "baboon", "time_rad"])

#Test Statistic: 0.0095 
#P-value > 0.10 

baboon_overlap_coeffs<-as.data.frame(cbind(baboon_gt_ResNet18_raw.overlap,baboon_gt_ResNet50_raw.overlap,baboon_gt_ConvNextT_raw.overlap))
baboon_overlap_coeffs$species<-"baboon"
plot(gt_raw_baboon, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.15), las=1, lwd=2)
plot(ResNet18_raw_baboon, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_baboon, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_baboon, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.87(0.86-1.01)", "ResNet50 Δ4=0.88(0.89-1.05)","ConvNextT Δ4=0.88(0.88-1.04)"), col=1:4, lty=1, pch=21, cex=0.5, bty="n")

title("baboon-no threshold")

#buffalo-done ##194 gt records #####
bs.buffalo.gt<-resample(gt_raw[gt_raw$label == "buffalo", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_buffalo_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                                          ResNet18_raw[ResNet18_raw$pred1_label == "buffalo", "time_rad"]))
bs.buffalo.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "buffalo", "time_rad"],10000)
bsbuffalo_gt_ResNet18_raw<-bootEst(bs.buffalo.gt,bs.buffalo.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsbuffalo_gt_ResNet18_raw.mean <-as.vector(colMeans(bsbuffalo_gt_ResNet18_raw))
bsbuffalo_gt_ResNet18_raw.mean <-bsbuffalo_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsbuffalo_gt_ResNet18_rawVec<-as.vector(bsbuffalo_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsbuffalo_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_buffalo_gt_ResNet18_raw[2],bsbuffalo_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
buffalo_gt_ResNet18_raw.overlap <- cbind(bsbuffalo_gt_ResNet18_raw.mean,bsbuffalo_gt_ResNet18_raw.CI)

buffalo_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                                               ResNet18_raw[ResNet18_raw$pred1_label == "buffalo", "time_rad"])
#Test Statistic: 0.0927 
#P-value > 0.10

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_buffalo_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                                          ResNet50_raw[ResNet50_raw$pred1_label == "buffalo", "time_rad"]))

bs.buffalo.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "buffalo", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsbuffalo_gt_ResNet50_raw<-bootEst(bs.buffalo.gt,bs.buffalo.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsbuffalo_gt_ResNet50_raw.mean <-as.vector(colMeans(bsbuffalo_gt_ResNet50_raw))
bsbuffalo_gt_ResNet50_raw.mean <-bsbuffalo_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsbuffalo_gt_ResNet50_rawVec<-as.vector(bsbuffalo_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsbuffalo_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_buffalo_gt_ResNet50_raw[2],bsbuffalo_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
buffalo_gt_ResNet50_raw.overlap <- cbind(bsbuffalo_gt_ResNet50_raw.mean,bsbuffalo_gt_ResNet50_raw.CI)

buffalo_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                                               ResNet50_raw[ResNet50_raw$pred1_label == "buffalo", "time_rad"])
#Test Statistic: 0.0927
#P-value > 0.10

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_buffalo_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                                           ConvNextT_raw[ConvNextT_raw$pred1_label == "buffalo", "time_rad"]))

bs.buffalo.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "buffalo", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsbuffalo_gt_ConvNextT_raw<-bootEst(bs.buffalo.gt,bs.buffalo.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsbuffalo_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsbuffalo_gt_ConvNextT_raw))
bsbuffalo_gt_ConvNextT_raw.mean <-bsbuffalo_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsbuffalo_gt_ConvNextT_rawVec<-as.vector(bsbuffalo_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsbuffalo_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_buffalo_gt_ConvNextT_raw[2],bsbuffalo_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
buffalo_gt_ConvNextT_raw.overlap <- cbind(bsbuffalo_gt_ConvNextT_raw.mean,bsbuffalo_gt_ConvNextT_raw.CI)
#
buffalo_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "buffalo", "time_rad"],
                                                ConvNextT_raw[ConvNextT_raw$pred1_label == "buffalo", "time_rad"])

#Test Statistic: 0.1266 
#P-value > 0.10 

buffalo_overlap_coeffs<-as.data.frame(cbind(buffalo_gt_ResNet18_raw.overlap,buffalo_gt_ResNet50_raw.overlap,buffalo_gt_ConvNextT_raw.overlap))
buffalo_overlap_coeffs$species<-"buffalo"
plot(gt_raw_buffalo, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.2), las=1, lwd=2)
plot(ResNet18_raw_buffalo, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_buffalo, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_buffalo, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.82(0.78-0.91)", "ResNet50 Δ4=0.81(0.76-0.88)","ConvNextT Δ4=0.81(0.76-0.90)"), col=1:4, lty=1, pch=21, cex=0.5, bty="n")

title("buffalo-no threshold")

#dikdik-done ##154 gt records #####
bs.dikdik.gt<-resample(gt_raw[gt_raw$label == "dikdik", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_dikdik_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                                           ResNet18_raw[ResNet18_raw$pred1_label == "dikdik", "time_rad"]))
bs.dikdik.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "dikdik", "time_rad"],10000)
bsdikdik_gt_ResNet18_raw<-bootEst(bs.dikdik.gt,bs.dikdik.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsdikdik_gt_ResNet18_raw.mean <-as.vector(colMeans(bsdikdik_gt_ResNet18_raw))
bsdikdik_gt_ResNet18_raw.mean <-bsdikdik_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsdikdik_gt_ResNet18_rawVec<-as.vector(bsdikdik_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsdikdik_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_dikdik_gt_ResNet18_raw[2],bsdikdik_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
dikdik_gt_ResNet18_raw.overlap <- cbind(bsdikdik_gt_ResNet18_raw.mean,bsdikdik_gt_ResNet18_raw.CI)

dikdik_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                                                ResNet18_raw[ResNet18_raw$pred1_label == "dikdik", "time_rad"])
#Test Statistic: 0.0157
#P-value > 0.10

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_dikdik_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                                           ResNet50_raw[ResNet50_raw$pred1_label == "dikdik", "time_rad"]))

bs.dikdik.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "dikdik", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsdikdik_gt_ResNet50_raw<-bootEst(bs.dikdik.gt,bs.dikdik.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsdikdik_gt_ResNet50_raw.mean <-as.vector(colMeans(bsdikdik_gt_ResNet50_raw))
bsdikdik_gt_ResNet50_raw.mean <-bsdikdik_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsdikdik_gt_ResNet50_rawVec<-as.vector(bsdikdik_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsdikdik_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_dikdik_gt_ResNet50_raw[2],bsdikdik_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
dikdik_gt_ResNet50_raw.overlap <- cbind(bsdikdik_gt_ResNet50_raw.mean,bsdikdik_gt_ResNet50_raw.CI)

dikdik_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                                                ResNet50_raw[ResNet50_raw$pred1_label == "dikdik", "time_rad"])
#Test Statistic: 0.0354 
#P-value > 0.10

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_dikdik_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                                            ConvNextT_raw[ConvNextT_raw$pred1_label == "dikdik", "time_rad"]))

bs.dikdik.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "dikdik", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsdikdik_gt_ConvNextT_raw<-bootEst(bs.dikdik.gt,bs.dikdik.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsdikdik_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsdikdik_gt_ConvNextT_raw))
bsdikdik_gt_ConvNextT_raw.mean <-bsdikdik_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsdikdik_gt_ConvNextT_rawVec<-as.vector(bsdikdik_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsdikdik_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_dikdik_gt_ConvNextT_raw[2],bsdikdik_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
dikdik_gt_ConvNextT_raw.overlap <- cbind(bsdikdik_gt_ConvNextT_raw.mean,bsdikdik_gt_ConvNextT_raw.CI)
#
dikdik_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "dikdik", "time_rad"],
                                                 ConvNextT_raw[ConvNextT_raw$pred1_label == "dikdik", "time_rad"])

#Test Statistic: 0.0325 
#P-value > 0.10 

dikdik_overlap_coeffs<-as.data.frame(cbind(dikdik_gt_ResNet18_raw.overlap,dikdik_gt_ResNet50_raw.overlap,dikdik_gt_ConvNextT_raw.overlap))
dikdik_overlap_coeffs$species<-"dikdik"
plot(gt_raw_dikdik, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_dikdik, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_dikdik, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_dikdik, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.87(0.87-1.00)", "ResNet50 Δ4=0.86(0.84-0.97)","ConvNextT Δ4=0.87(0.85-0.98)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")

title("dikdik-no threshold")
#eland-done ##103 gt records #####
bs.eland.gt<-resample(gt_raw[gt_raw$label == "eland", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_eland_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "eland", "time_rad"],
                                          ResNet18_raw[ResNet18_raw$pred1_label == "eland", "time_rad"]))
bs.eland.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "eland", "time_rad"],10000)
bseland_gt_ResNet18_raw<-bootEst(bs.eland.gt,bs.eland.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bseland_gt_ResNet18_raw.mean <-as.vector(colMeans(bseland_gt_ResNet18_raw))
bseland_gt_ResNet18_raw.mean <-bseland_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bseland_gt_ResNet18_rawVec<-as.vector(bseland_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bseland_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_eland_gt_ResNet18_raw[2],bseland_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
eland_gt_ResNet18_raw.overlap <- cbind(bseland_gt_ResNet18_raw.mean,bseland_gt_ResNet18_raw.CI)

eland_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "eland", "time_rad"],
                                               ResNet18_raw[ResNet18_raw$pred1_label == "eland", "time_rad"])
#Test Statistic: 0.1564 
#0.05 < P-value < 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_eland_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "eland", "time_rad"],
                                          ResNet50_raw[ResNet50_raw$pred1_label == "eland", "time_rad"]))

bs.eland.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "eland", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bseland_gt_ResNet50_raw<-bootEst(bs.eland.gt,bs.eland.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bseland_gt_ResNet50_raw.mean <-as.vector(colMeans(bseland_gt_ResNet50_raw))
bseland_gt_ResNet50_raw.mean <-bseland_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bseland_gt_ResNet50_rawVec<-as.vector(bseland_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bseland_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_eland_gt_ResNet50_raw[2],bseland_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
eland_gt_ResNet50_raw.overlap <- cbind(bseland_gt_ResNet50_raw.mean,bseland_gt_ResNet50_raw.CI)

eland_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "eland", "time_rad"],
                                               ResNet50_raw[ResNet50_raw$pred1_label == "eland", "time_rad"])
#Test Statistic: 0.1861 
#0.05 < P-value < 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_eland_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "eland", "time_rad"],
                                           ConvNextT_raw[ConvNextT_raw$pred1_label == "eland", "time_rad"]))

bs.eland.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "eland", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bseland_gt_ConvNextT_raw<-bootEst(bs.eland.gt,bs.eland.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bseland_gt_ConvNextT_raw.mean <-as.vector(colMeans(bseland_gt_ConvNextT_raw))
bseland_gt_ConvNextT_raw.mean <-bseland_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bseland_gt_ConvNextT_rawVec<-as.vector(bseland_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bseland_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_eland_gt_ConvNextT_raw[2],bseland_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
eland_gt_ConvNextT_raw.overlap <- cbind(bseland_gt_ConvNextT_raw.mean,bseland_gt_ConvNextT_raw.CI)
#
eland_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "eland", "time_rad"],
                                                ConvNextT_raw[ConvNextT_raw$pred1_label == "eland", "time_rad"])

#Test Statistic: 0.1546 
#0.05 < P-value < 0.10 

eland_overlap_coeffs<-as.data.frame(cbind(eland_gt_ResNet18_raw.overlap,eland_gt_ResNet50_raw.overlap,eland_gt_ConvNextT_raw.overlap))
eland_overlap_coeffs$species<-"eland"
plot(gt_raw_eland, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_eland, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_eland, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_eland, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.82(0.75-0.94)", "ResNet50 Δ4=0.83(0.74-0.94)","ConvNextT Δ4=0.84(0.77-0.95)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")

title("eland-no threshold")
#elephant-done  #####
bs.elephant.gt<-resample(gt_raw[gt_raw$label == "elephant", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_elephant_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "elephant", "time_rad"],
                                         ResNet18_raw[ResNet18_raw$pred1_label == "elephant", "time_rad"]))
bs.elephant.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "elephant", "time_rad"],10000)
bselephant_gt_ResNet18_raw<-bootEst(bs.elephant.gt,bs.elephant.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bselephant_gt_ResNet18_raw.mean <-as.vector(colMeans(bselephant_gt_ResNet18_raw))
bselephant_gt_ResNet18_raw.mean <-bselephant_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bselephant_gt_ResNet18_rawVec<-as.vector(bselephant_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bselephant_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_elephant_gt_ResNet18_raw[2],bselephant_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
elephant_gt_ResNet18_raw.overlap <- cbind(bselephant_gt_ResNet18_raw.mean,bselephant_gt_ResNet18_raw.CI)

elephant_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "elephant", "time_rad"],
                                              ResNet18_raw[ResNet18_raw$pred1_label == "elephant", "time_rad"])
#Test Statistic: 0.0225 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_elephant_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "elephant", "time_rad"],
                                         ResNet50_raw[ResNet50_raw$pred1_label == "elephant", "time_rad"]))

bs.elephant.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "elephant", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bselephant_gt_ResNet50_raw<-bootEst(bs.elephant.gt,bs.elephant.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bselephant_gt_ResNet50_raw.mean <-as.vector(colMeans(bselephant_gt_ResNet50_raw))
bselephant_gt_ResNet50_raw.mean <-bselephant_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bselephant_gt_ResNet50_rawVec<-as.vector(bselephant_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bselephant_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_elephant_gt_ResNet50_raw[2],bselephant_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
elephant_gt_ResNet50_raw.overlap <- cbind(bselephant_gt_ResNet50_raw.mean,bselephant_gt_ResNet50_raw.CI)

elephant_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "elephant", "time_rad"],
                                              ResNet50_raw[ResNet50_raw$pred1_label == "elephant", "time_rad"])
#Test Statistic: 0.0249 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_elephant_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "elephant", "time_rad"],
                                          ConvNextT_raw[ConvNextT_raw$pred1_label == "elephant", "time_rad"]))

bs.elephant.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "elephant", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bselephant_gt_ConvNextT_raw<-bootEst(bs.elephant.gt,bs.elephant.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bselephant_gt_ConvNextT_raw.mean <-as.vector(colMeans(bselephant_gt_ConvNextT_raw))
bselephant_gt_ConvNextT_raw.mean <-bselephant_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bselephant_gt_ConvNextT_rawVec<-as.vector(bselephant_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bselephant_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_elephant_gt_ConvNextT_raw[2],bselephant_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
elephant_gt_ConvNextT_raw.overlap <- cbind(bselephant_gt_ConvNextT_raw.mean,bselephant_gt_ConvNextT_raw.CI)
#
elephant_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "elephant", "time_rad"],
                                               ConvNextT_raw[ConvNextT_raw$pred1_label == "elephant", "time_rad"])

#Test Statistic: 0.0314 
#P-value > 0.10 

elephant_overlap_coeffs<-as.data.frame(cbind(elephant_gt_ResNet18_raw.overlap,elephant_gt_ResNet50_raw.overlap,elephant_gt_ConvNextT_raw.overlap))
elephant_overlap_coeffs$species<-"elephant"
plot(gt_raw_elephant, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_elephant, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_elephant, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_elephant, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.91(0.90-1.00)", "ResNet50 Δ4=0.91(0.89-1.00)","ConvNextT Δ4=0.90(0.89-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")

title("elephant-no threshold")
#gazelle_grants-done  #####
bs.gazelle_grants.gt<-resample(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_gazelle_grants_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                                            ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_grants", "time_rad"]))
bs.gazelle_grants.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_grants", "time_rad"],10000)
bsgazelle_grants_gt_ResNet18_raw<-bootEst(bs.gazelle_grants.gt,bs.gazelle_grants.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgazelle_grants_gt_ResNet18_raw.mean <-as.vector(colMeans(bsgazelle_grants_gt_ResNet18_raw))
bsgazelle_grants_gt_ResNet18_raw.mean <-bsgazelle_grants_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgazelle_grants_gt_ResNet18_rawVec<-as.vector(bsgazelle_grants_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgazelle_grants_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_gazelle_grants_gt_ResNet18_raw[2],bsgazelle_grants_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
gazelle_grants_gt_ResNet18_raw.overlap <- cbind(bsgazelle_grants_gt_ResNet18_raw.mean,bsgazelle_grants_gt_ResNet18_raw.CI)

gazelle_grants_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                                                 ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_grants", "time_rad"])
#Test Statistic: 0.0479  
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_gazelle_grants_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                                            ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_grants", "time_rad"]))

bs.gazelle_grants.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_grants", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsgazelle_grants_gt_ResNet50_raw<-bootEst(bs.gazelle_grants.gt,bs.gazelle_grants.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgazelle_grants_gt_ResNet50_raw.mean <-as.vector(colMeans(bsgazelle_grants_gt_ResNet50_raw))
bsgazelle_grants_gt_ResNet50_raw.mean <-bsgazelle_grants_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgazelle_grants_gt_ResNet50_rawVec<-as.vector(bsgazelle_grants_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgazelle_grants_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_gazelle_grants_gt_ResNet50_raw[2],bsgazelle_grants_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
gazelle_grants_gt_ResNet50_raw.overlap <- cbind(bsgazelle_grants_gt_ResNet50_raw.mean,bsgazelle_grants_gt_ResNet50_raw.CI)

gazelle_grants_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                                                 ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_grants", "time_rad"])
#Test Statistic: 0.0343 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_gazelle_grants_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                                             ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_grants", "time_rad"]))

bs.gazelle_grants.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_grants", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsgazelle_grants_gt_ConvNextT_raw<-bootEst(bs.gazelle_grants.gt,bs.gazelle_grants.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgazelle_grants_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsgazelle_grants_gt_ConvNextT_raw))
bsgazelle_grants_gt_ConvNextT_raw.mean <-bsgazelle_grants_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgazelle_grants_gt_ConvNextT_rawVec<-as.vector(bsgazelle_grants_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgazelle_grants_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_gazelle_grants_gt_ConvNextT_raw[2],bsgazelle_grants_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
gazelle_grants_gt_ConvNextT_raw.overlap <- cbind(bsgazelle_grants_gt_ConvNextT_raw.mean,bsgazelle_grants_gt_ConvNextT_raw.CI)
#
gazelle_grants_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "gazelle_grants", "time_rad"],
                                                  ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_grants", "time_rad"])

#Test Statistic: 0.0434 
#P-value > 0.10 

gazelle_grants_overlap_coeffs<-as.data.frame(cbind(gazelle_grants_gt_ResNet18_raw.overlap,gazelle_grants_gt_ResNet50_raw.overlap,gazelle_grants_gt_ConvNextT_raw.overlap))
gazelle_grants_overlap_coeffs$species<-"gazelle_grants"
plot(gt_raw_gazelle_grants, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.15), las=1, lwd=2)
plot(ResNet18_raw_gazelle_grants, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_gazelle_grants, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_gazelle_grants, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.84(0.81-0.95)", "ResNet50 Δ4=0.85(0.83-0.96)","ConvNextT Δ4=0.86(0.84-0.97)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
gazelle_grants_overlap_coeffs_rounded<-library(dplyr); gazelle_grants_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)


title("gazelle_grants-no threshold")
#gazelle_thomsons-done  #####
bs.gazelle_thomsons.gt<-resample(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_gazelle_thomsons_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                                                  ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_thomsons", "time_rad"]))
bs.gazelle_thomsons.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_thomsons", "time_rad"],10000)
bsgazelle_thomsons_gt_ResNet18_raw<-bootEst(bs.gazelle_thomsons.gt,bs.gazelle_thomsons.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgazelle_thomsons_gt_ResNet18_raw.mean <-as.vector(colMeans(bsgazelle_thomsons_gt_ResNet18_raw))
bsgazelle_thomsons_gt_ResNet18_raw.mean <-bsgazelle_thomsons_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgazelle_thomsons_gt_ResNet18_rawVec<-as.vector(bsgazelle_thomsons_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgazelle_thomsons_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_gazelle_thomsons_gt_ResNet18_raw[2],bsgazelle_thomsons_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
gazelle_thomsons_gt_ResNet18_raw.overlap <- cbind(bsgazelle_thomsons_gt_ResNet18_raw.mean,bsgazelle_thomsons_gt_ResNet18_raw.CI)

gazelle_thomsons_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                                                       ResNet18_raw[ResNet18_raw$pred1_label == "gazelle_thomsons", "time_rad"])
#Test Statistic: 0.0879  
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_gazelle_thomsons_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                                                  ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_thomsons", "time_rad"]))

bs.gazelle_thomsons.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_thomsons", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsgazelle_thomsons_gt_ResNet50_raw<-bootEst(bs.gazelle_thomsons.gt,bs.gazelle_thomsons.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgazelle_thomsons_gt_ResNet50_raw.mean <-as.vector(colMeans(bsgazelle_thomsons_gt_ResNet50_raw))
bsgazelle_thomsons_gt_ResNet50_raw.mean <-bsgazelle_thomsons_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgazelle_thomsons_gt_ResNet50_rawVec<-as.vector(bsgazelle_thomsons_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgazelle_thomsons_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_gazelle_thomsons_gt_ResNet50_raw[2],bsgazelle_thomsons_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
gazelle_thomsons_gt_ResNet50_raw.overlap <- cbind(bsgazelle_thomsons_gt_ResNet50_raw.mean,bsgazelle_thomsons_gt_ResNet50_raw.CI)

gazelle_thomsons_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                                                       ResNet50_raw[ResNet50_raw$pred1_label == "gazelle_thomsons", "time_rad"])
#Test Statistic: 0.048
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_gazelle_thomsons_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                                                   ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_thomsons", "time_rad"]))

bs.gazelle_thomsons.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_thomsons", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsgazelle_thomsons_gt_ConvNextT_raw<-bootEst(bs.gazelle_thomsons.gt,bs.gazelle_thomsons.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgazelle_thomsons_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsgazelle_thomsons_gt_ConvNextT_raw))
bsgazelle_thomsons_gt_ConvNextT_raw.mean <-bsgazelle_thomsons_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgazelle_thomsons_gt_ConvNextT_rawVec<-as.vector(bsgazelle_thomsons_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgazelle_thomsons_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_gazelle_thomsons_gt_ConvNextT_raw[2],bsgazelle_thomsons_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
gazelle_thomsons_gt_ConvNextT_raw.overlap <- cbind(bsgazelle_thomsons_gt_ConvNextT_raw.mean,bsgazelle_thomsons_gt_ConvNextT_raw.CI)
#
gazelle_thomsons_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "gazelle_thomsons", "time_rad"],
                                                        ConvNextT_raw[ConvNextT_raw$pred1_label == "gazelle_thomsons", "time_rad"])

#Test Statistic: 0.0489 
#P-value > 0.10 

gazelle_thomsons_overlap_coeffs<-as.data.frame(cbind(gazelle_thomsons_gt_ResNet18_raw.overlap,gazelle_thomsons_gt_ResNet50_raw.overlap,gazelle_thomsons_gt_ConvNextT_raw.overlap))
gazelle_thomsons_overlap_coeffs$species<-"gazelle_thomsons"
plot(gt_raw_gazelle_thomsons, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_gazelle_thomsons, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_gazelle_thomsons, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_gazelle_thomsons, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.96(0.96-0.99)", "ResNet50 Δ4=0.96(0.96-0.99)","ConvNextT Δ4=0.97(0.96-0.99)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
gazelle_thomsons_overlap_coeffs_rounded<-library(dplyr); gazelle_thomsons_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("gazelle_thomsons-no threshold")


#giraffe-done  #####
bs.giraffe.gt<-resample(gt_raw[gt_raw$label == "giraffe", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_giraffe_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                                                    ResNet18_raw[ResNet18_raw$pred1_label == "giraffe", "time_rad"]))
bs.giraffe.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "giraffe", "time_rad"],10000)
bsgiraffe_gt_ResNet18_raw<-bootEst(bs.giraffe.gt,bs.giraffe.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgiraffe_gt_ResNet18_raw.mean <-as.vector(colMeans(bsgiraffe_gt_ResNet18_raw))
bsgiraffe_gt_ResNet18_raw.mean <-bsgiraffe_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgiraffe_gt_ResNet18_rawVec<-as.vector(bsgiraffe_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgiraffe_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_giraffe_gt_ResNet18_raw[2],bsgiraffe_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
giraffe_gt_ResNet18_raw.overlap <- cbind(bsgiraffe_gt_ResNet18_raw.mean,bsgiraffe_gt_ResNet18_raw.CI)

giraffe_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                                                         ResNet18_raw[ResNet18_raw$pred1_label == "giraffe", "time_rad"])
#Test Statistic:  0.0095  
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_giraffe_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                                                    ResNet50_raw[ResNet50_raw$pred1_label == "giraffe", "time_rad"]))

bs.giraffe.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "giraffe", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsgiraffe_gt_ResNet50_raw<-bootEst(bs.giraffe.gt,bs.giraffe.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgiraffe_gt_ResNet50_raw.mean <-as.vector(colMeans(bsgiraffe_gt_ResNet50_raw))
bsgiraffe_gt_ResNet50_raw.mean <-bsgiraffe_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgiraffe_gt_ResNet50_rawVec<-as.vector(bsgiraffe_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgiraffe_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_giraffe_gt_ResNet50_raw[2],bsgiraffe_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
giraffe_gt_ResNet50_raw.overlap <- cbind(bsgiraffe_gt_ResNet50_raw.mean,bsgiraffe_gt_ResNet50_raw.CI)

giraffe_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                                                         ResNet50_raw[ResNet50_raw$pred1_label == "giraffe", "time_rad"])
#Test Statistic: 0.0126 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_giraffe_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                                                     ConvNextT_raw[ConvNextT_raw$pred1_label == "giraffe", "time_rad"]))

bs.giraffe.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "giraffe", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsgiraffe_gt_ConvNextT_raw<-bootEst(bs.giraffe.gt,bs.giraffe.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsgiraffe_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsgiraffe_gt_ConvNextT_raw))
bsgiraffe_gt_ConvNextT_raw.mean <-bsgiraffe_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsgiraffe_gt_ConvNextT_rawVec<-as.vector(bsgiraffe_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsgiraffe_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_giraffe_gt_ConvNextT_raw[2],bsgiraffe_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
giraffe_gt_ConvNextT_raw.overlap <- cbind(bsgiraffe_gt_ConvNextT_raw.mean,bsgiraffe_gt_ConvNextT_raw.CI)
#
giraffe_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "giraffe", "time_rad"],
                                                          ConvNextT_raw[ConvNextT_raw$pred1_label == "giraffe", "time_rad"])

#Test Statistic: 0.0081  
#P-value > 0.10 

giraffe_overlap_coeffs<-as.data.frame(cbind(giraffe_gt_ResNet18_raw.overlap,giraffe_gt_ResNet50_raw.overlap,giraffe_gt_ConvNextT_raw.overlap))
giraffe_overlap_coeffs$species<-"giraffe"
plot(gt_raw_giraffe, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_giraffe, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_giraffe, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_giraffe, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.91(0.92-1.01)", "ResNet50 Δ4=0.91(0.92-1.01)","ConvNextT Δ4=0.91(0.93-1.02)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
giraffe_overlap_coeffs_rounded<-library(dplyr); giraffe_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("giraffe-no threshold")
#hare-done  #####
bs.hare.gt<-resample(gt_raw[gt_raw$label == "hare", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_hare_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "hare", "time_rad"],
                                           ResNet18_raw[ResNet18_raw$pred1_label == "hare", "time_rad"]))
bs.hare.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "hare", "time_rad"],10000)
bshare_gt_ResNet18_raw<-bootEst(bs.hare.gt,bs.hare.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bshare_gt_ResNet18_raw.mean <-as.vector(colMeans(bshare_gt_ResNet18_raw))
bshare_gt_ResNet18_raw.mean <-bshare_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshare_gt_ResNet18_rawVec<-as.vector(bshare_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshare_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_hare_gt_ResNet18_raw[2],bshare_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
hare_gt_ResNet18_raw.overlap <- cbind(bshare_gt_ResNet18_raw.mean,bshare_gt_ResNet18_raw.CI)

hare_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hare", "time_rad"],
                                                ResNet18_raw[ResNet18_raw$pred1_label == "hare", "time_rad"])
#Test Statistic:  0.0366
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_hare_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "hare", "time_rad"],
                                           ResNet50_raw[ResNet50_raw$pred1_label == "hare", "time_rad"]))

bs.hare.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "hare", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshare_gt_ResNet50_raw<-bootEst(bs.hare.gt,bs.hare.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bshare_gt_ResNet50_raw.mean <-as.vector(colMeans(bshare_gt_ResNet50_raw))
bshare_gt_ResNet50_raw.mean <-bshare_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshare_gt_ResNet50_rawVec<-as.vector(bshare_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshare_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_hare_gt_ResNet50_raw[2],bshare_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
hare_gt_ResNet50_raw.overlap <- cbind(bshare_gt_ResNet50_raw.mean,bshare_gt_ResNet50_raw.CI)

hare_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hare", "time_rad"],
                                                ResNet50_raw[ResNet50_raw$pred1_label == "hare", "time_rad"])
#Test Statistic: 0.0288 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_hare_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "hare", "time_rad"],
                                            ConvNextT_raw[ConvNextT_raw$pred1_label == "hare", "time_rad"]))

bs.hare.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "hare", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshare_gt_ConvNextT_raw<-bootEst(bs.hare.gt,bs.hare.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bshare_gt_ConvNextT_raw.mean <-as.vector(colMeans(bshare_gt_ConvNextT_raw))
bshare_gt_ConvNextT_raw.mean <-bshare_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshare_gt_ConvNextT_rawVec<-as.vector(bshare_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshare_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_hare_gt_ConvNextT_raw[2],bshare_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
hare_gt_ConvNextT_raw.overlap <- cbind(bshare_gt_ConvNextT_raw.mean,bshare_gt_ConvNextT_raw.CI)
#
hare_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hare", "time_rad"],
                                                 ConvNextT_raw[ConvNextT_raw$pred1_label == "hare", "time_rad"])

#Test Statistic: 0.0189  
#P-value > 0.10 

hare_overlap_coeffs<-as.data.frame(cbind(hare_gt_ResNet18_raw.overlap,hare_gt_ResNet50_raw.overlap,hare_gt_ConvNextT_raw.overlap))
hare_overlap_coeffs$species<-"hare"
plot(gt_raw_hare, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_hare, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_hare, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_hare, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("top",c("groundtruth", "ResNet18 Δ4=0.91(0.88-1.00)", "ResNet50 Δ4=0.91(0.88-1.00)","ConvNextT Δ4=0.91(0.90-1.01)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
hare_overlap_coeffs_rounded<-library(dplyr); hare_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("hare-no threshold")


#hartebeest_cokes - done  #####
bs.hartebeest_cokes.gt<-resample(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_hartebeest_cokes_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                                        ResNet18_raw[ResNet18_raw$pred1_label == "hartebeest_cokes", "time_rad"]))
bs.hartebeest_cokes.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "hartebeest_cokes", "time_rad"],10000)
bshartebeest_cokes_gt_ResNet18_raw<-bootEst(bs.hartebeest_cokes.gt,bs.hartebeest_cokes.ResNet18_raw,adjust=c(0.8,NA,NA)) #Dhat 4
bshartebeest_cokes_gt_ResNet18_raw.mean <-as.vector(colMeans(bshartebeest_cokes_gt_ResNet18_raw))
bshartebeest_cokes_gt_ResNet18_raw.mean <-bshartebeest_cokes_gt_ResNet18_raw.mean[1]
#Convert column with Dhat4 to vector and get CIs
bshartebeest_cokes_gt_ResNet18_rawVec<-as.vector(bshartebeest_cokes_gt_ResNet18_raw[,1])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshartebeest_cokes_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_hartebeest_cokes_gt_ResNet18_raw[1],bshartebeest_cokes_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
hartebeest_cokes_gt_ResNet18_raw.overlap <- cbind(bshartebeest_cokes_gt_ResNet18_raw.mean,bshartebeest_cokes_gt_ResNet18_raw.CI)

hartebeest_cokes_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                                             ResNet18_raw[ResNet18_raw$pred1_label == "hartebeest_cokes", "time_rad"])
#Test Statistic:  0.0366
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_hartebeest_cokes_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                                        ResNet50_raw[ResNet50_raw$pred1_label == "hartebeest_cokes", "time_rad"]))

bs.hartebeest_cokes.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "hartebeest_cokes", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshartebeest_cokes_gt_ResNet50_raw<-bootEst(bs.hartebeest_cokes.gt,bs.hartebeest_cokes.ResNet50_raw,adjust=c(0.8,NA,NA)) #Dhat 4
bshartebeest_cokes_gt_ResNet50_raw.mean <-as.vector(colMeans(bshartebeest_cokes_gt_ResNet50_raw))
bshartebeest_cokes_gt_ResNet50_raw.mean <-bshartebeest_cokes_gt_ResNet50_raw.mean[1]
#Convert column with Dhat4 to vector and get CIs
bshartebeest_cokes_gt_ResNet50_rawVec<-as.vector(bshartebeest_cokes_gt_ResNet50_raw[,1])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshartebeest_cokes_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_hartebeest_cokes_gt_ResNet50_raw[1],bshartebeest_cokes_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
hartebeest_cokes_gt_ResNet50_raw.overlap <- cbind(bshartebeest_cokes_gt_ResNet50_raw.mean,bshartebeest_cokes_gt_ResNet50_raw.CI)

hartebeest_cokes_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                                             ResNet50_raw[ResNet50_raw$pred1_label == "hartebeest_cokes", "time_rad"])
#Test Statistic: 0.0288 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_hartebeest_cokes_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                                         ConvNextT_raw[ConvNextT_raw$pred1_label == "hartebeest_cokes", "time_rad"]))

bs.hartebeest_cokes.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "hartebeest_cokes", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshartebeest_cokes_gt_ConvNextT_raw<-bootEst(bs.hartebeest_cokes.gt,bs.hartebeest_cokes.ConvNextT_raw,adjust=c(0.8,NA,NA)) #Dhat 4
bshartebeest_cokes_gt_ConvNextT_raw.mean <-as.vector(colMeans(bshartebeest_cokes_gt_ConvNextT_raw))
bshartebeest_cokes_gt_ConvNextT_raw.mean <-bshartebeest_cokes_gt_ConvNextT_raw.mean[1]
#Convert column with Dhat4 to vector and get CIs
bshartebeest_cokes_gt_ConvNextT_rawVec<-as.vector(bshartebeest_cokes_gt_ConvNextT_raw[,1])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshartebeest_cokes_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_hartebeest_cokes_gt_ConvNextT_raw[1],bshartebeest_cokes_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
hartebeest_cokes_gt_ConvNextT_raw.overlap <- cbind(bshartebeest_cokes_gt_ConvNextT_raw.mean,bshartebeest_cokes_gt_ConvNextT_raw.CI)
#
hartebeest_cokes_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hartebeest_cokes", "time_rad"],
                                              ConvNextT_raw[ConvNextT_raw$pred1_label == "hartebeest_cokes", "time_rad"])

#Test Statistic: 0.0189  
#P-value > 0.10 

hartebeest_cokes_overlap_coeffs<-as.data.frame(cbind(hartebeest_cokes_gt_ResNet18_raw.overlap,hartebeest_cokes_gt_ResNet50_raw.overlap,hartebeest_cokes_gt_ConvNextT_raw.overlap))
hartebeest_cokes_overlap_coeffs$species<-"hartebeest_cokes"
plot(gt_raw_hartebeest_cokes, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_hartebeest_cokes, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_hartebeest_cokes, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_hartebeest_cokes, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("top",c("groundtruth", "ResNet18 Δ4=0.91(0.88-1.00)", "ResNet50 Δ4=0.91(0.88-1.00)","ConvNextT Δ4=0.91(0.90-1.01)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
hartebeest_cokes_overlap_coeffs_rounded<-library(dplyr); hartebeest_cokes_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("hartebeest_cokes-no threshold")

#hippopotamus-done  #####
bs.hippopotamus.gt<-resample(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_hippopotamus_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                                        ResNet18_raw[ResNet18_raw$pred1_label == "hippopotamus", "time_rad"]))
bs.hippopotamus.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "hippopotamus", "time_rad"],10000)
bshippopotamus_gt_ResNet18_raw<-bootEst(bs.hippopotamus.gt,bs.hippopotamus.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bshippopotamus_gt_ResNet18_raw.mean <-as.vector(colMeans(bshippopotamus_gt_ResNet18_raw))
bshippopotamus_gt_ResNet18_raw.mean <-bshippopotamus_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshippopotamus_gt_ResNet18_rawVec<-as.vector(bshippopotamus_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshippopotamus_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_hippopotamus_gt_ResNet18_raw[2],bshippopotamus_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
hippopotamus_gt_ResNet18_raw.overlap <- cbind(bshippopotamus_gt_ResNet18_raw.mean,bshippopotamus_gt_ResNet18_raw.CI)

hippopotamus_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                                             ResNet18_raw[ResNet18_raw$pred1_label == "hippopotamus", "time_rad"])
#Test Statistic:  0.0415 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_hippopotamus_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                                        ResNet50_raw[ResNet50_raw$pred1_label == "hippopotamus", "time_rad"]))

bs.hippopotamus.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "hippopotamus", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshippopotamus_gt_ResNet50_raw<-bootEst(bs.hippopotamus.gt,bs.hippopotamus.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bshippopotamus_gt_ResNet50_raw.mean <-as.vector(colMeans(bshippopotamus_gt_ResNet50_raw))
bshippopotamus_gt_ResNet50_raw.mean <-bshippopotamus_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshippopotamus_gt_ResNet50_rawVec<-as.vector(bshippopotamus_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshippopotamus_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_hippopotamus_gt_ResNet50_raw[2],bshippopotamus_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
hippopotamus_gt_ResNet50_raw.overlap <- cbind(bshippopotamus_gt_ResNet50_raw.mean,bshippopotamus_gt_ResNet50_raw.CI)

hippopotamus_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                                             ResNet50_raw[ResNet50_raw$pred1_label == "hippopotamus", "time_rad"])
#Test Statistic: 0.0284 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_hippopotamus_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                                         ConvNextT_raw[ConvNextT_raw$pred1_label == "hippopotamus", "time_rad"]))

bs.hippopotamus.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "hippopotamus", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshippopotamus_gt_ConvNextT_raw<-bootEst(bs.hippopotamus.gt,bs.hippopotamus.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bshippopotamus_gt_ConvNextT_raw.mean <-as.vector(colMeans(bshippopotamus_gt_ConvNextT_raw))
bshippopotamus_gt_ConvNextT_raw.mean <-bshippopotamus_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshippopotamus_gt_ConvNextT_rawVec<-as.vector(bshippopotamus_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshippopotamus_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_hippopotamus_gt_ConvNextT_raw[2],bshippopotamus_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
hippopotamus_gt_ConvNextT_raw.overlap <- cbind(bshippopotamus_gt_ConvNextT_raw.mean,bshippopotamus_gt_ConvNextT_raw.CI)
#
hippopotamus_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hippopotamus", "time_rad"],
                                              ConvNextT_raw[ConvNextT_raw$pred1_label == "hippopotamus", "time_rad"])

#Test Statistic: 0.0196 
#P-value > 0.10 

hippopotamus_overlap_coeffs<-as.data.frame(cbind(hippopotamus_gt_ResNet18_raw.overlap,hippopotamus_gt_ResNet50_raw.overlap,hippopotamus_gt_ConvNextT_raw.overlap))
hippopotamus_overlap_coeffs$species<-"hippopotamus"
plot(gt_raw_hippopotamus, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_hippopotamus, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_hippopotamus, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_hippopotamus, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("top",c("groundtruth", "ResNet18 Δ4=0.88(0.80-0.98)", "ResNet50 Δ4=0.88(0.82-0.99)","ConvNextT Δ4=0.88(0.84-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
hippopotamus_overlap_coeffs_rounded<-library(dplyr); hippopotamus_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("hippopotamus-no threshold")
#hyena_aardwolf-done  #####
bs.hyena_aardwolf.gt<-resample(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_hyena_aardwolf_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                                                ResNet18_raw[ResNet18_raw$pred1_label == "hyena_aardwolf", "time_rad"]))
bs.hyena_aardwolf.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "hyena_aardwolf", "time_rad"],10000)
bshyena_aardwolf_gt_ResNet18_raw<-bootEst(bs.hyena_aardwolf.gt,bs.hyena_aardwolf.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bshyena_aardwolf_gt_ResNet18_raw.mean <-as.vector(colMeans(bshyena_aardwolf_gt_ResNet18_raw))
bshyena_aardwolf_gt_ResNet18_raw.mean <-bshyena_aardwolf_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshyena_aardwolf_gt_ResNet18_rawVec<-as.vector(bshyena_aardwolf_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshyena_aardwolf_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_hyena_aardwolf_gt_ResNet18_raw[2],bshyena_aardwolf_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
hyena_aardwolf_gt_ResNet18_raw.overlap <- cbind(bshyena_aardwolf_gt_ResNet18_raw.mean,bshyena_aardwolf_gt_ResNet18_raw.CI)

hyena_aardwolf_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                                                     ResNet18_raw[ResNet18_raw$pred1_label == "hyena_aardwolf", "time_rad"])
#Test Statistic:   0.0662 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_hyena_aardwolf_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                                                ResNet50_raw[ResNet50_raw$pred1_label == "hyena_aardwolf", "time_rad"]))

bs.hyena_aardwolf.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "hyena_aardwolf", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshyena_aardwolf_gt_ResNet50_raw<-bootEst(bs.hyena_aardwolf.gt,bs.hyena_aardwolf.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bshyena_aardwolf_gt_ResNet50_raw.mean <-as.vector(colMeans(bshyena_aardwolf_gt_ResNet50_raw))
bshyena_aardwolf_gt_ResNet50_raw.mean <-bshyena_aardwolf_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshyena_aardwolf_gt_ResNet50_rawVec<-as.vector(bshyena_aardwolf_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshyena_aardwolf_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_hyena_aardwolf_gt_ResNet50_raw[2],bshyena_aardwolf_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
hyena_aardwolf_gt_ResNet50_raw.overlap <- cbind(bshyena_aardwolf_gt_ResNet50_raw.mean,bshyena_aardwolf_gt_ResNet50_raw.CI)

hyena_aardwolf_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                                                     ResNet50_raw[ResNet50_raw$pred1_label == "hyena_aardwolf", "time_rad"])
#Test Statistic:0.0321 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_hyena_aardwolf_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                                                 ConvNextT_raw[ConvNextT_raw$pred1_label == "hyena_aardwolf", "time_rad"]))

bs.hyena_aardwolf.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "hyena_aardwolf", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bshyena_aardwolf_gt_ConvNextT_raw<-bootEst(bs.hyena_aardwolf.gt,bs.hyena_aardwolf.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bshyena_aardwolf_gt_ConvNextT_raw.mean <-as.vector(colMeans(bshyena_aardwolf_gt_ConvNextT_raw))
bshyena_aardwolf_gt_ConvNextT_raw.mean <-bshyena_aardwolf_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bshyena_aardwolf_gt_ConvNextT_rawVec<-as.vector(bshyena_aardwolf_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bshyena_aardwolf_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_hyena_aardwolf_gt_ConvNextT_raw[2],bshyena_aardwolf_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
hyena_aardwolf_gt_ConvNextT_raw.overlap <- cbind(bshyena_aardwolf_gt_ConvNextT_raw.mean,bshyena_aardwolf_gt_ConvNextT_raw.CI)
#
hyena_aardwolf_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "hyena_aardwolf", "time_rad"],
                                                      ConvNextT_raw[ConvNextT_raw$pred1_label == "hyena_aardwolf", "time_rad"])

#Test Statistic: 0.0296  
#P-value > 0.10 

hyena_aardwolf_overlap_coeffs<-as.data.frame(cbind(hyena_aardwolf_gt_ResNet18_raw.overlap,hyena_aardwolf_gt_ResNet50_raw.overlap,hyena_aardwolf_gt_ConvNextT_raw.overlap))
hyena_aardwolf_overlap_coeffs$species<-"hyena_aardwolf"
plot(gt_raw_hyena_aardwolf, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_hyena_aardwolf, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_hyena_aardwolf, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_hyena_aardwolf, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("top",c("groundtruth", "ResNet18 Δ4=0.91(0.88-0.98)", "ResNet50 Δ4=0.91(0.90-0.99)","ConvNextT Δ4=0.91(0.90-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
hyena_aardwolf_overlap_coeffs_rounded<-library(dplyr); hyena_aardwolf_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("hyena_aardwolf-no threshold")
#impala-done  #####
bs.impala.gt<-resample(gt_raw[gt_raw$label == "impala", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_impala_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "impala", "time_rad"],
                                                  ResNet18_raw[ResNet18_raw$pred1_label == "impala", "time_rad"]))
bs.impala.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "impala", "time_rad"],10000)
bsimpala_gt_ResNet18_raw<-bootEst(bs.impala.gt,bs.impala.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsimpala_gt_ResNet18_raw.mean <-as.vector(colMeans(bsimpala_gt_ResNet18_raw))
bsimpala_gt_ResNet18_raw.mean <-bsimpala_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsimpala_gt_ResNet18_rawVec<-as.vector(bsimpala_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsimpala_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_impala_gt_ResNet18_raw[2],bsimpala_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
impala_gt_ResNet18_raw.overlap <- cbind(bsimpala_gt_ResNet18_raw.mean,bsimpala_gt_ResNet18_raw.CI)

impala_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "impala", "time_rad"],
                                                       ResNet18_raw[ResNet18_raw$pred1_label == "impala", "time_rad"])
#Test Statistic:   0.0271 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_impala_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "impala", "time_rad"],
                                                  ResNet50_raw[ResNet50_raw$pred1_label == "impala", "time_rad"]))

bs.impala.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "impala", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsimpala_gt_ResNet50_raw<-bootEst(bs.impala.gt,bs.impala.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsimpala_gt_ResNet50_raw.mean <-as.vector(colMeans(bsimpala_gt_ResNet50_raw))
bsimpala_gt_ResNet50_raw.mean <-bsimpala_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsimpala_gt_ResNet50_rawVec<-as.vector(bsimpala_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsimpala_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_impala_gt_ResNet50_raw[2],bsimpala_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
impala_gt_ResNet50_raw.overlap <- cbind(bsimpala_gt_ResNet50_raw.mean,bsimpala_gt_ResNet50_raw.CI)

impala_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "impala", "time_rad"],
                                                       ResNet50_raw[ResNet50_raw$pred1_label == "impala", "time_rad"])
#Test Statistic:0.0251
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_impala_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "impala", "time_rad"],
                                                   ConvNextT_raw[ConvNextT_raw$pred1_label == "impala", "time_rad"]))

bs.impala.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "impala", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsimpala_gt_ConvNextT_raw<-bootEst(bs.impala.gt,bs.impala.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsimpala_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsimpala_gt_ConvNextT_raw))
bsimpala_gt_ConvNextT_raw.mean <-bsimpala_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsimpala_gt_ConvNextT_rawVec<-as.vector(bsimpala_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsimpala_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_impala_gt_ConvNextT_raw[2],bsimpala_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
impala_gt_ConvNextT_raw.overlap <- cbind(bsimpala_gt_ConvNextT_raw.mean,bsimpala_gt_ConvNextT_raw.CI)
#
impala_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "impala", "time_rad"],
                                                        ConvNextT_raw[ConvNextT_raw$pred1_label == "impala", "time_rad"])

#Test Statistic: 0.0194  
#P-value > 0.10 

impala_overlap_coeffs<-as.data.frame(cbind(impala_gt_ResNet18_raw.overlap,impala_gt_ResNet50_raw.overlap,impala_gt_ConvNextT_raw.overlap))
impala_overlap_coeffs$species<-"impala"
plot(gt_raw_impala, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_impala, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_impala, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_impala, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.96(0.96-0.99)", "ResNet50 Δ4=0.96(0.96-0.99)","ConvNextT Δ4=0.96(0.96-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
impala_overlap_coeffs_rounded<-library(dplyr); impala_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("impala-no threshold")
#jackal-done  #####
bs.jackal.gt<-resample(gt_raw[gt_raw$label == "jackal", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_jackal_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "jackal", "time_rad"],
                                          ResNet18_raw[ResNet18_raw$pred1_label == "jackal", "time_rad"]))
bs.jackal.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "jackal", "time_rad"],10000)
bsjackal_gt_ResNet18_raw<-bootEst(bs.jackal.gt,bs.jackal.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsjackal_gt_ResNet18_raw.mean <-as.vector(colMeans(bsjackal_gt_ResNet18_raw))
bsjackal_gt_ResNet18_raw.mean <-bsjackal_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsjackal_gt_ResNet18_rawVec<-as.vector(bsjackal_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsjackal_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_jackal_gt_ResNet18_raw[2],bsjackal_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
jackal_gt_ResNet18_raw.overlap <- cbind(bsjackal_gt_ResNet18_raw.mean,bsjackal_gt_ResNet18_raw.CI)

jackal_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "jackal", "time_rad"],
                                               ResNet18_raw[ResNet18_raw$pred1_label == "jackal", "time_rad"])
#Test Statistic:   0.0133 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_jackal_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "jackal", "time_rad"],
                                          ResNet50_raw[ResNet50_raw$pred1_label == "jackal", "time_rad"]))

bs.jackal.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "jackal", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsjackal_gt_ResNet50_raw<-bootEst(bs.jackal.gt,bs.jackal.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsjackal_gt_ResNet50_raw.mean <-as.vector(colMeans(bsjackal_gt_ResNet50_raw))
bsjackal_gt_ResNet50_raw.mean <-bsjackal_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsjackal_gt_ResNet50_rawVec<-as.vector(bsjackal_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsjackal_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_jackal_gt_ResNet50_raw[2],bsjackal_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
jackal_gt_ResNet50_raw.overlap <- cbind(bsjackal_gt_ResNet50_raw.mean,bsjackal_gt_ResNet50_raw.CI)

jackal_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "jackal", "time_rad"],
                                               ResNet50_raw[ResNet50_raw$pred1_label == "jackal", "time_rad"])
#Test Statistic:0.0224
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_jackal_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "jackal", "time_rad"],
                                           ConvNextT_raw[ConvNextT_raw$pred1_label == "jackal", "time_rad"]))

bs.jackal.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "jackal", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsjackal_gt_ConvNextT_raw<-bootEst(bs.jackal.gt,bs.jackal.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsjackal_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsjackal_gt_ConvNextT_raw))
bsjackal_gt_ConvNextT_raw.mean <-bsjackal_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsjackal_gt_ConvNextT_rawVec<-as.vector(bsjackal_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsjackal_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_jackal_gt_ConvNextT_raw[2],bsjackal_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
jackal_gt_ConvNextT_raw.overlap <- cbind(bsjackal_gt_ConvNextT_raw.mean,bsjackal_gt_ConvNextT_raw.CI)
#
jackal_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "jackal", "time_rad"],
                                                ConvNextT_raw[ConvNextT_raw$pred1_label == "jackal", "time_rad"])

#Test Statistic: 0.0175 
#P-value > 0.10 

jackal_overlap_coeffs<-as.data.frame(cbind(jackal_gt_ResNet18_raw.overlap,jackal_gt_ResNet50_raw.overlap,jackal_gt_ConvNextT_raw.overlap))
jackal_overlap_coeffs$species<-"jackal"
plot(gt_raw_jackal, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.1), las=1, lwd=2)
plot(ResNet18_raw_jackal, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_jackal, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_jackal, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("top",c("groundtruth", "ResNet18 Δ4=0.88(0.89-1.03)", "ResNet50 Δ4=0.87(0.86-1.01)","ConvNextT Δ4=0.87(0.86-1.01)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
jackal_overlap_coeffs_rounded<-library(dplyr); jackal_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("jackal-no threshold")
#mongoose-done  #####
bs.mongoose.gt<-resample(gt_raw[gt_raw$label == "mongoose", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_mongoose_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                                          ResNet18_raw[ResNet18_raw$pred1_label == "mongoose", "time_rad"]))
bs.mongoose.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "mongoose", "time_rad"],10000)
bsmongoose_gt_ResNet18_raw<-bootEst(bs.mongoose.gt,bs.mongoose.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bsmongoose_gt_ResNet18_raw.mean <-as.vector(colMeans(bsmongoose_gt_ResNet18_raw))
bsmongoose_gt_ResNet18_raw.mean <-bsmongoose_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsmongoose_gt_ResNet18_rawVec<-as.vector(bsmongoose_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsmongoose_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_mongoose_gt_ResNet18_raw[2],bsmongoose_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
mongoose_gt_ResNet18_raw.overlap <- cbind(bsmongoose_gt_ResNet18_raw.mean,bsmongoose_gt_ResNet18_raw.CI)

mongoose_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                                               ResNet18_raw[ResNet18_raw$pred1_label == "mongoose", "time_rad"])
#Test Statistic:0.0304 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_mongoose_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                                          ResNet50_raw[ResNet50_raw$pred1_label == "mongoose", "time_rad"]))

bs.mongoose.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "mongoose", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsmongoose_gt_ResNet50_raw<-bootEst(bs.mongoose.gt,bs.mongoose.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bsmongoose_gt_ResNet50_raw.mean <-as.vector(colMeans(bsmongoose_gt_ResNet50_raw))
bsmongoose_gt_ResNet50_raw.mean <-bsmongoose_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsmongoose_gt_ResNet50_rawVec<-as.vector(bsmongoose_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsmongoose_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_mongoose_gt_ResNet50_raw[2],bsmongoose_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
mongoose_gt_ResNet50_raw.overlap <- cbind(bsmongoose_gt_ResNet50_raw.mean,bsmongoose_gt_ResNet50_raw.CI)

mongoose_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                                               ResNet50_raw[ResNet50_raw$pred1_label == "mongoose", "time_rad"])
#Test Statistic:0.0221
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_mongoose_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                                           ConvNextT_raw[ConvNextT_raw$pred1_label == "mongoose", "time_rad"]))

bs.mongoose.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "mongoose", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsmongoose_gt_ConvNextT_raw<-bootEst(bs.mongoose.gt,bs.mongoose.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bsmongoose_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsmongoose_gt_ConvNextT_raw))
bsmongoose_gt_ConvNextT_raw.mean <-bsmongoose_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bsmongoose_gt_ConvNextT_rawVec<-as.vector(bsmongoose_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsmongoose_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_mongoose_gt_ConvNextT_raw[2],bsmongoose_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
mongoose_gt_ConvNextT_raw.overlap <- cbind(bsmongoose_gt_ConvNextT_raw.mean,bsmongoose_gt_ConvNextT_raw.CI)
#
mongoose_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "mongoose", "time_rad"],
                                                ConvNextT_raw[ConvNextT_raw$pred1_label == "mongoose", "time_rad"])

#Test Statistic: 0.0205  
#P-value > 0.10 

mongoose_overlap_coeffs<-as.data.frame(cbind(mongoose_gt_ResNet18_raw.overlap,mongoose_gt_ResNet50_raw.overlap,mongoose_gt_ConvNextT_raw.overlap))
mongoose_overlap_coeffs$species<-"mongoose"
plot(gt_raw_mongoose, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.1), las=1, lwd=2)
plot(ResNet18_raw_mongoose, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_mongoose, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_mongoose, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.87(0.84-0.97)", "ResNet50 Δ4=0.87(0.85-0.98)","ConvNextT Δ4=0.87(0.86-0.99)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
mongoose_overlap_coeffs_rounded<-library(dplyr); mongoose_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("mongoose-no threshold")
#topi-done  #####
bs.topi.gt<-resample(gt_raw[gt_raw$label == "topi", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_topi_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "topi", "time_rad"],
                                            ResNet18_raw[ResNet18_raw$pred1_label == "topi", "time_rad"]))
bs.topi.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "topi", "time_rad"],10000)
bstopi_gt_ResNet18_raw<-bootEst(bs.topi.gt,bs.topi.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bstopi_gt_ResNet18_raw.mean <-as.vector(colMeans(bstopi_gt_ResNet18_raw))
bstopi_gt_ResNet18_raw.mean <-bstopi_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bstopi_gt_ResNet18_rawVec<-as.vector(bstopi_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bstopi_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_topi_gt_ResNet18_raw[2],bstopi_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
topi_gt_ResNet18_raw.overlap <- cbind(bstopi_gt_ResNet18_raw.mean,bstopi_gt_ResNet18_raw.CI)

topi_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "topi", "time_rad"],
                                                 ResNet18_raw[ResNet18_raw$pred1_label == "topi", "time_rad"])
#Test Statistic:0.0145
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_topi_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "topi", "time_rad"],
                                            ResNet50_raw[ResNet50_raw$pred1_label == "topi", "time_rad"]))

bs.topi.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "topi", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bstopi_gt_ResNet50_raw<-bootEst(bs.topi.gt,bs.topi.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bstopi_gt_ResNet50_raw.mean <-as.vector(colMeans(bstopi_gt_ResNet50_raw))
bstopi_gt_ResNet50_raw.mean <-bstopi_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bstopi_gt_ResNet50_rawVec<-as.vector(bstopi_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bstopi_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_topi_gt_ResNet50_raw[2],bstopi_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
topi_gt_ResNet50_raw.overlap <- cbind(bstopi_gt_ResNet50_raw.mean,bstopi_gt_ResNet50_raw.CI)

topi_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "topi", "time_rad"],
                                                 ResNet50_raw[ResNet50_raw$pred1_label == "topi", "time_rad"])
#Test Statistic:0.012
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_topi_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "topi", "time_rad"],
                                             ConvNextT_raw[ConvNextT_raw$pred1_label == "topi", "time_rad"]))

bs.topi.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "topi", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bstopi_gt_ConvNextT_raw<-bootEst(bs.topi.gt,bs.topi.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bstopi_gt_ConvNextT_raw.mean <-as.vector(colMeans(bstopi_gt_ConvNextT_raw))
bstopi_gt_ConvNextT_raw.mean <-bstopi_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bstopi_gt_ConvNextT_rawVec<-as.vector(bstopi_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bstopi_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_topi_gt_ConvNextT_raw[2],bstopi_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
topi_gt_ConvNextT_raw.overlap <- cbind(bstopi_gt_ConvNextT_raw.mean,bstopi_gt_ConvNextT_raw.CI)
#
topi_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "topi", "time_rad"],
                                                  ConvNextT_raw[ConvNextT_raw$pred1_label == "topi", "time_rad"])

#Test Statistic: 0.0115
#P-value > 0.10 

topi_overlap_coeffs<-as.data.frame(cbind(topi_gt_ResNet18_raw.overlap,topi_gt_ResNet50_raw.overlap,topi_gt_ConvNextT_raw.overlap))
topi_overlap_coeffs$species<-"topi"
plot(gt_raw_topi, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_topi, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_topi, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_topi, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.95(0.95-1.00)", "ResNet50 Δ4=0.95(0.95-1.00)","ConvNextT Δ4=0.96(0.96-1.01)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
topi_overlap_coeffs_rounded<-library(dplyr); topi_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("topi-no threshold")
#vervet monkey- not done####
bs.vervet_monkey.gt<-resample(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_vervet_monkey_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                                              ResNet18_raw[ResNet18_raw$pred1_label == "vervet_monkey", "time_rad"]))
bs.vervet_monkey.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "vervet_monkey", "time_rad"],10000)
bsvervet_monkey_gt_ResNet18_raw<-bootEst(bs.vervet_monkey.gt,bs.vervet_monkey.ResNet18_raw,adjust=c(0.8,NA,NA)) #Dhat 4
bsvervet_monkey_gt_ResNet18_raw.mean <-as.vector(colMeans(bsvervet_monkey_gt_ResNet18_raw))
bsvervet_monkey_gt_ResNet18_raw.mean <-bsvervet_monkey_gt_ResNet18_raw.mean[1]
#Convert column with Dhat4 to vector and get CIs
bsvervet_monkey_gt_ResNet18_rawVec<-as.vector(bsvervet_monkey_gt_ResNet18_raw[,1])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsvervet_monkey_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_vervet_monkey_gt_ResNet18_raw[1],bsvervet_monkey_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
vervet_monkey_gt_ResNet18_raw.overlap <- cbind(bsvervet_monkey_gt_ResNet18_raw.mean,bsvervet_monkey_gt_ResNet18_raw.CI)

vervet_monkey_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                                                   ResNet18_raw[ResNet18_raw$pred1_label == "vervet_monkey", "time_rad"])
#Test Statistic: 0.0318 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_vervet_monkey_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                                              ResNet50_raw[ResNet50_raw$pred1_label == "vervet_monkey", "time_rad"]))

bs.vervet_monkey.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "vervet_monkey", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsvervet_monkey_gt_ResNet50_raw<-bootEst(bs.vervet_monkey.gt,bs.vervet_monkey.ResNet50_raw,adjust=c(0.8,NA,NA)) #Dhat 4
bsvervet_monkey_gt_ResNet50_raw.mean <-as.vector(colMeans(bsvervet_monkey_gt_ResNet50_raw))
bsvervet_monkey_gt_ResNet50_raw.mean <-bsvervet_monkey_gt_ResNet50_raw.mean[1]
#Convert column with Dhat4 to vector and get CIs
bsvervet_monkey_gt_ResNet50_rawVec<-as.vector(bsvervet_monkey_gt_ResNet50_raw[,1])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsvervet_monkey_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_vervet_monkey_gt_ResNet50_raw[1],bsvervet_monkey_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
vervet_monkey_gt_ResNet50_raw.overlap <- cbind(bsvervet_monkey_gt_ResNet50_raw.mean,bsvervet_monkey_gt_ResNet50_raw.CI)

vervet_monkey_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                                                   ResNet50_raw[ResNet50_raw$pred1_label == "vervet_monkey", "time_rad"])
#Test Statistic:0.0325 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_vervet_monkey_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                                               ConvNextT_raw[ConvNextT_raw$pred1_label == "vervet_monkey", "time_rad"]))

bs.vervet_monkey.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "vervet_monkey", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bsvervet_monkey_gt_ConvNextT_raw<-bootEst(bs.vervet_monkey.gt,bs.vervet_monkey.ConvNextT_raw,adjust=c(0.8,NA,NA)) #Dhat 4
bsvervet_monkey_gt_ConvNextT_raw.mean <-as.vector(colMeans(bsvervet_monkey_gt_ConvNextT_raw))
bsvervet_monkey_gt_ConvNextT_raw.mean <-bsvervet_monkey_gt_ConvNextT_raw.mean[1]
#Convert column with Dhat4 to vector and get CIs
bsvervet_monkey_gt_ConvNextT_rawVec<-as.vector(bsvervet_monkey_gt_ConvNextT_raw[,1])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bsvervet_monkey_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_vervet_monkey_gt_ConvNextT_raw[1],bsvervet_monkey_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
vervet_monkey_gt_ConvNextT_raw.overlap <- cbind(bsvervet_monkey_gt_ConvNextT_raw.mean,bsvervet_monkey_gt_ConvNextT_raw.CI)
#
vervet_monkey_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "vervet_monkey", "time_rad"],
                                                    ConvNextT_raw[ConvNextT_raw$pred1_label == "vervet_monkey", "time_rad"])

#Test Statistic: 0.0131 
#P-value > 0.10 

vervet_monkey_overlap_coeffs<-as.data.frame(cbind(vervet_monkey_gt_ResNet18_raw.overlap,vervet_monkey_gt_ResNet50_raw.overlap,vervet_monkey_gt_ConvNextT_raw.overlap))
vervet_monkey_overlap_coeffs$species<-"vervet_monkey"
plot(gt_raw_vervet_monkey, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_vervet_monkey, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_vervet_monkey, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_vervet_monkey, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.98(0.97-0.99)", "ResNet50 Δ4=0.98(0.97-0.99)","ConvNextT Δ4=0.98(0.98-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
vervet_monkey_overlap_coeffs_rounded<-library(dplyr); vervet_monkey_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("vervet_monkey-no threshold")
#warthog- done####
bs.warthog.gt<-resample(gt_raw[gt_raw$label == "warthog", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_warthog_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "warthog", "time_rad"],
                                        ResNet18_raw[ResNet18_raw$pred1_label == "warthog", "time_rad"]))
bs.warthog.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "warthog", "time_rad"],10000)
bswarthog_gt_ResNet18_raw<-bootEst(bs.warthog.gt,bs.warthog.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bswarthog_gt_ResNet18_raw.mean <-as.vector(colMeans(bswarthog_gt_ResNet18_raw))
bswarthog_gt_ResNet18_raw.mean <-bswarthog_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bswarthog_gt_ResNet18_rawVec<-as.vector(bswarthog_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bswarthog_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_warthog_gt_ResNet18_raw[2],bswarthog_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
warthog_gt_ResNet18_raw.overlap <- cbind(bswarthog_gt_ResNet18_raw.mean,bswarthog_gt_ResNet18_raw.CI)

warthog_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "warthog", "time_rad"],
                                             ResNet18_raw[ResNet18_raw$pred1_label == "warthog", "time_rad"])
#Test Statistic:0.0548
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_warthog_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "warthog", "time_rad"],
                                        ResNet50_raw[ResNet50_raw$pred1_label == "warthog", "time_rad"]))

bs.warthog.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "warthog", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bswarthog_gt_ResNet50_raw<-bootEst(bs.warthog.gt,bs.warthog.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bswarthog_gt_ResNet50_raw.mean <-as.vector(colMeans(bswarthog_gt_ResNet50_raw))
bswarthog_gt_ResNet50_raw.mean <-bswarthog_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bswarthog_gt_ResNet50_rawVec<-as.vector(bswarthog_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bswarthog_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_warthog_gt_ResNet50_raw[2],bswarthog_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
warthog_gt_ResNet50_raw.overlap <- cbind(bswarthog_gt_ResNet50_raw.mean,bswarthog_gt_ResNet50_raw.CI)

warthog_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "warthog", "time_rad"],
                                             ResNet50_raw[ResNet50_raw$pred1_label == "warthog", "time_rad"])
#Test Statistic:0.0313 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_warthog_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "warthog", "time_rad"],
                                         ConvNextT_raw[ConvNextT_raw$pred1_label == "warthog", "time_rad"]))

bs.warthog.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "warthog", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bswarthog_gt_ConvNextT_raw<-bootEst(bs.warthog.gt,bs.warthog.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bswarthog_gt_ConvNextT_raw.mean <-as.vector(colMeans(bswarthog_gt_ConvNextT_raw))
bswarthog_gt_ConvNextT_raw.mean <-bswarthog_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bswarthog_gt_ConvNextT_rawVec<-as.vector(bswarthog_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bswarthog_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_warthog_gt_ConvNextT_raw[2],bswarthog_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
warthog_gt_ConvNextT_raw.overlap <- cbind(bswarthog_gt_ConvNextT_raw.mean,bswarthog_gt_ConvNextT_raw.CI)
#
warthog_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "warthog", "time_rad"],
                                              ConvNextT_raw[ConvNextT_raw$pred1_label == "warthog", "time_rad"])

#Test Statistic: 0.0368 
#P-value > 0.10 

warthog_overlap_coeffs<-as.data.frame(cbind(warthog_gt_ResNet18_raw.overlap,warthog_gt_ResNet50_raw.overlap,warthog_gt_ConvNextT_raw.overlap))
warthog_overlap_coeffs$species<-"warthog"
plot(gt_raw_warthog, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_warthog, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_warthog, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_warthog, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topright",c("groundtruth", "ResNet18 Δ4=0.93(0.91-0.98)", "ResNet50 Δ4=0.93(0.93-0.99)","ConvNextT Δ4=0.94(0.92-0.99)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
warthog_overlap_coeffs_rounded<-library(dplyr); warthog_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("warthog-no threshold")

#wildebeest -done####
bs.wildebeest.gt<-resample(gt_raw[gt_raw$label == "wildebeest", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_wildebeest_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                                           ResNet18_raw[ResNet18_raw$pred1_label == "wildebeest", "time_rad"]))
bs.wildebeest.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "wildebeest", "time_rad"],10000)
bswildebeest_gt_ResNet18_raw<-bootEst(bs.wildebeest.gt,bs.wildebeest.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bswildebeest_gt_ResNet18_raw.mean <-as.vector(colMeans(bswildebeest_gt_ResNet18_raw))
bswildebeest_gt_ResNet18_raw.mean <-bswildebeest_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bswildebeest_gt_ResNet18_rawVec<-as.vector(bswildebeest_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bswildebeest_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_wildebeest_gt_ResNet18_raw[2],bswildebeest_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
wildebeest_gt_ResNet18_raw.overlap <- cbind(bswildebeest_gt_ResNet18_raw.mean,bswildebeest_gt_ResNet18_raw.CI)

wildebeest_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                                                ResNet18_raw[ResNet18_raw$pred1_label == "wildebeest", "time_rad"])
#Test Statistic: 0.0318 
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_wildebeest_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                                           ResNet50_raw[ResNet50_raw$pred1_label == "wildebeest", "time_rad"]))

bs.wildebeest.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "wildebeest", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bswildebeest_gt_ResNet50_raw<-bootEst(bs.wildebeest.gt,bs.wildebeest.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bswildebeest_gt_ResNet50_raw.mean <-as.vector(colMeans(bswildebeest_gt_ResNet50_raw))
bswildebeest_gt_ResNet50_raw.mean <-bswildebeest_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bswildebeest_gt_ResNet50_rawVec<-as.vector(bswildebeest_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bswildebeest_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_wildebeest_gt_ResNet50_raw[2],bswildebeest_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
wildebeest_gt_ResNet50_raw.overlap <- cbind(bswildebeest_gt_ResNet50_raw.mean,bswildebeest_gt_ResNet50_raw.CI)

wildebeest_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                                                ResNet50_raw[ResNet50_raw$pred1_label == "wildebeest", "time_rad"])
#Test Statistic:0.0325 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_wildebeest_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                                            ConvNextT_raw[ConvNextT_raw$pred1_label == "wildebeest", "time_rad"]))

bs.wildebeest.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "wildebeest", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bswildebeest_gt_ConvNextT_raw<-bootEst(bs.wildebeest.gt,bs.wildebeest.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bswildebeest_gt_ConvNextT_raw.mean <-as.vector(colMeans(bswildebeest_gt_ConvNextT_raw))
bswildebeest_gt_ConvNextT_raw.mean <-bswildebeest_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bswildebeest_gt_ConvNextT_rawVec<-as.vector(bswildebeest_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bswildebeest_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_wildebeest_gt_ConvNextT_raw[2],bswildebeest_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
wildebeest_gt_ConvNextT_raw.overlap <- cbind(bswildebeest_gt_ConvNextT_raw.mean,bswildebeest_gt_ConvNextT_raw.CI)
#
wildebeest_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "wildebeest", "time_rad"],
                                                 ConvNextT_raw[ConvNextT_raw$pred1_label == "wildebeest", "time_rad"])

#Test Statistic: 0.0131 
#P-value > 0.10 

wildebeest_overlap_coeffs<-as.data.frame(cbind(wildebeest_gt_ResNet18_raw.overlap,wildebeest_gt_ResNet50_raw.overlap,wildebeest_gt_ConvNextT_raw.overlap))
wildebeest_overlap_coeffs$species<-"wildebeest"
plot(gt_raw_wildebeest, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.12), las=1, lwd=2)
plot(ResNet18_raw_wildebeest, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_wildebeest, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_wildebeest, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.98(0.97-0.99)", "ResNet50 Δ4=0.98(0.97-0.99)","ConvNextT Δ4=0.98(0.98-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
wildebeest_overlap_coeffs_rounded<-library(dplyr); wildebeest_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("wildebeest-no threshold")
#zebra- done####
bs.zebra.gt<-resample(gt_raw[gt_raw$label == "zebra", "time_rad"],10000) 

###ResNet18 Dhat###
#estimating Dhats - overlap metric
(Dhats_zebra_gt_ResNet18_raw<-overlapEst(gt_raw[gt_raw$label == "zebra", "time_rad"],
                                              ResNet18_raw[ResNet18_raw$pred1_label == "zebra", "time_rad"]))
bs.zebra.ResNet18_raw<-resample(ResNet18_raw[ResNet18_raw$pred1_label == "zebra", "time_rad"],10000)
bszebra_gt_ResNet18_raw<-bootEst(bs.zebra.gt,bs.zebra.ResNet18_raw,adjust=c(NA,1,NA)) #Dhat 4
bszebra_gt_ResNet18_raw.mean <-as.vector(colMeans(bszebra_gt_ResNet18_raw))
bszebra_gt_ResNet18_raw.mean <-bszebra_gt_ResNet18_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bszebra_gt_ResNet18_rawVec<-as.vector(bszebra_gt_ResNet18_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bszebra_gt_ResNet18_raw.CI <- as.vector(bootCI(Dhats_zebra_gt_ResNet18_raw[2],bszebra_gt_ResNet18_rawVec)['basic0',]) #2 because it's Dhat 4
zebra_gt_ResNet18_raw.overlap <- cbind(bszebra_gt_ResNet18_raw.mean,bszebra_gt_ResNet18_raw.CI)

zebra_gt_ResNet18_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "zebra", "time_rad"],
                                                   ResNet18_raw[ResNet18_raw$pred1_label == "zebra", "time_rad"])
#Test Statistic: 0.038
#P-value > 0.10 

###ResNet50###
#estimating Dhats - overlap metric
(Dhats_zebra_gt_ResNet50_raw<-overlapEst(gt_raw[gt_raw$label == "zebra", "time_rad"],
                                              ResNet50_raw[ResNet50_raw$pred1_label == "zebra", "time_rad"]))

bs.zebra.ResNet50_raw<-resample(ResNet50_raw[ResNet50_raw$pred1_label == "zebra", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bszebra_gt_ResNet50_raw<-bootEst(bs.zebra.gt,bs.zebra.ResNet50_raw,adjust=c(NA,1,NA)) #Dhat 4
bszebra_gt_ResNet50_raw.mean <-as.vector(colMeans(bszebra_gt_ResNet50_raw))
bszebra_gt_ResNet50_raw.mean <-bszebra_gt_ResNet50_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bszebra_gt_ResNet50_rawVec<-as.vector(bszebra_gt_ResNet50_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bszebra_gt_ResNet50_raw.CI <- as.vector(bootCI(Dhats_zebra_gt_ResNet50_raw[2],bszebra_gt_ResNet50_rawVec)['basic0',]) #2 because it's Dhat 4
zebra_gt_ResNet50_raw.overlap <- cbind(bszebra_gt_ResNet50_raw.mean,bszebra_gt_ResNet50_raw.CI)

zebra_gt_ResNet50_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "zebra", "time_rad"],
                                                   ResNet50_raw[ResNet50_raw$pred1_label == "zebra", "time_rad"])
#Test Statistic:0.0278 
#P-value > 0.10 

###ConvNextT###
#estimating Dhats - overlap metric
(Dhats_zebra_gt_ConvNextT_raw<-overlapEst(gt_raw[gt_raw$label == "zebra", "time_rad"],
                                               ConvNextT_raw[ConvNextT_raw$pred1_label == "zebra", "time_rad"]))

bs.zebra.ConvNextT_raw<-resample(ConvNextT_raw[ConvNextT_raw$pred1_label == "zebra", "time_rad"],10000)
## When smaller sample <50, Dhat1 performed best, while Dhat4 was better when smaller sample >75 (Meredith & Ridout 2018; overview of overlap)
bszebra_gt_ConvNextT_raw<-bootEst(bs.zebra.gt,bs.zebra.ConvNextT_raw,adjust=c(NA,1,NA)) #Dhat 4
bszebra_gt_ConvNextT_raw.mean <-as.vector(colMeans(bszebra_gt_ConvNextT_raw))
bszebra_gt_ConvNextT_raw.mean <-bszebra_gt_ConvNextT_raw.mean[2]
#Convert column with Dhat4 to vector and get CIs
bszebra_gt_ConvNextT_rawVec<-as.vector(bszebra_gt_ConvNextT_raw[,2])#2 because it's Dhat 4
#bootCI(DhatsPtaj[2],bsPtajVec)['norm0',] #2 because it's Dhat 4
bszebra_gt_ConvNextT_raw.CI <- as.vector(bootCI(Dhats_zebra_gt_ConvNextT_raw[2],bszebra_gt_ConvNextT_rawVec)['basic0',]) #2 because it's Dhat 4
zebra_gt_ConvNextT_raw.overlap <- cbind(bszebra_gt_ConvNextT_raw.mean,bszebra_gt_ConvNextT_raw.CI)
#
zebra_gt_ConvNextT_raw_watson<-watson.two.test(gt_raw[gt_raw$label == "zebra", "time_rad"],
                                                    ConvNextT_raw[ConvNextT_raw$pred1_label == "zebra", "time_rad"])

#Test Statistic: 0.0231
#P-value > 0.10 

zebra_overlap_coeffs<-as.data.frame(cbind(zebra_gt_ResNet18_raw.overlap,zebra_gt_ResNet50_raw.overlap,zebra_gt_ConvNextT_raw.overlap))
zebra_overlap_coeffs$species<-"zebra"
plot(gt_raw_zebra, yunit="density", data="none", cline=list(col="white"),ylim=c(0,0.10), las=1, lwd=2)
plot(ResNet18_raw_zebra, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=2))
plot(ResNet50_raw_zebra, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=3))
plot(ConvNextT_raw_zebra, yunit="density", data="none", cline=list(col="white"),add=TRUE, tline=list(col=4))
legend("topleft",c("groundtruth", "ResNet18 Δ4=0.97(0.97-1.00)", "ResNet50 Δ4=0.97(0.97-1.00)","ConvNextT Δ4=0.97(0.98-1.00)"), col=1:4, lty=1, pch=21, cex=0.7, bty="n")
zebra_overlap_coeffs_rounded<-library(dplyr); zebra_overlap_coeffs %>% mutate_if(is.numeric, round, digits=2)
title("zebra-no threshold")
#combine and write####
baboon_overlap<-as.data.frame(cbind(baboon_gt_ResNet18_raw.overlap, baboon_gt_ResNet50_raw.overlap, baboon_gt_ConvNextT_raw.overlap))
baboon_overlap<-as.data.frame(cbind(baboon_gt_ResNet18_raw.overlap, baboon_gt_ResNet50_raw.overlap, baboon_gt_ConvNextT_raw.overlap))





baboon_watson<-cbind(baboon_gt_ResNet18_raw_watson, baboon_gt_ResNet50_raw_watson, baboon_gt_ConvNextT_raw_watson)

###Plotting####



##plotting overlapping activities
#overlapPlot(gt_raw[gt_raw$label == "baboon", "time_rad"],
#           ResNet18_raw[ResNet18_raw$pred1_label == "baboon", "time_rad"], 
#           xcenter= "noon",
#           rug=TRUE,
#           legend('topright', c("gt", "ResNet18"), lty=c(1, 2), col=c("black", "blue"), bty='n'),
#           main="baboon no threshold")
#

names(baboon_overlap_coeffs) = gsub(pattern = "bsbaboon_*", replacement = "", x = names(baboon_overlap_coeffs))

names(buffalo_overlap_coeffs) = gsub(pattern = "bsbuffalo_*", replacement = "", x = names(buffalo_overlap_coeffs))

names(dikdik_overlap_coeffs) = gsub(pattern = "bsdikdik_*", replacement = "", x = names(dikdik_overlap_coeffs))

names(eland_overlap_coeffs) = gsub(pattern = "bseland_*", replacement = "", x = names(eland_overlap_coeffs))

names(elephant_overlap_coeffs) = gsub(pattern = "bselephant_*", replacement = "", x = names(elephant_overlap_coeffs))

names(gazelle_grants_overlap_coeffs) = gsub(pattern = "bsgazelle_grants_*", replacement = "", x = names(gazelle_grants_overlap_coeffs))

names(gazelle_thomsons_overlap_coeffs) = gsub(pattern = "bsgazelle_thomsons_*", replacement = "", x = names(gazelle_thomsons_overlap_coeffs))

names(giraffe_overlap_coeffs) = gsub(pattern = "bsgiraffe_*", replacement = "", x = names(giraffe_overlap_coeffs))

names(hare_overlap_coeffs) = gsub(pattern = "bshare_*", replacement = "", x = names(hare_overlap_coeffs))

names(hartebeest_cokes_overlap_coeffs) = gsub(pattern = "bshartebeest_cokes_*", replacement = "", x = names(hartebeest_cokes_overlap_coeffs))

names(hippopotamus_overlap_coeffs) = gsub(pattern = "bshippopotamus_*", replacement = "", x = names(hippopotamus_overlap_coeffs))

names(hyena_aardwolf_overlap_coeffs) = gsub(pattern = "bshyena_aardwolf_*", replacement = "", x = names(hyena_aardwolf_overlap_coeffs))

names(impala_overlap_coeffs) = gsub(pattern = "bsimpala_*", replacement = "", x = names(impala_overlap_coeffs))

names(jackal_overlap_coeffs) = gsub(pattern = "bsjackal_*", replacement = "", x = names(jackal_overlap_coeffs))

names(mongoose_overlap_coeffs) = gsub(pattern = "bsmongoose_*", replacement = "", x = names(mongoose_overlap_coeffs))

names(topi_overlap_coeffs) = gsub(pattern = "bstopi_*", replacement = "", x = names(topi_overlap_coeffs))

names(vervet_monkey_overlap_coeffs) = gsub(pattern = "bsvervet_monkey_*", replacement = "", x = names(vervet_monkey_overlap_coeffs))

names(warthog_overlap_coeffs) = gsub(pattern = "bswarthog_*", replacement = "", x = names(warthog_overlap_coeffs))

names(wildebeest_overlap_coeffs) = gsub(pattern = "bswildebeest_*", replacement = "", x = names(wildebeest_overlap_coeffs))


names(zebra_overlap_coeffs) = gsub(pattern = "bszebra_*", replacement = "", x = names(zebra_overlap_coeffs))

all_overlap_coeffs<-rbind(baboon_overlap_coeffs, buffalo_overlap_coeffs, dikdik_overlap_coeffs, eland_overlap_coeffs, elephant_overlap_coeffs, gazelle_grants_overlap_coeffs, 
      gazelle_thomsons_overlap_coeffs, giraffe_overlap_coeffs, hare_overlap_coeffs,hartebeest_cokes_overlap_coeffs, hippopotamus_overlap_coeffs, 
      hyena_aardwolf_overlap_coeffs, impala_overlap_coeffs, jackal_overlap_coeffs, mongoose_overlap_coeffs, topi_overlap_coeffs, vervet_monkey_overlap_coeffs, warthog_overlap_coeffs, wildebeest_overlap_coeffs, zebra_overlap_coeffs)

write.csv(all_overlap_coeffs, "overlap_coeffs.csv")

library(tidyverse)

df_before <- tribble(~Singlecolumn,"Apple","Red","Banana","Yellow","Kiwi","Grey","Grapes","Green")

gt_ResNet50_raw.mean gt_ResNet50_raw.CI gt_ConvNextT_raw.mean gt_ConvNextT_raw.CI

all_overlap_coeffs2<-all_overlap_coeffs%>% 
  mutate(variable = rep(c("gt_ResNet18_raw.mean1","gt_ResNet18_raw.mean2","gt_ResNet18_raw.CI_lower", "gt_ResNet18_raw.CI_upper",
                          "gt_ResNet50_raw.mean1","gt_ResNet50_raw.mean2","gt_ResNet50_raw.CI_lower", "gt_ResNet50_raw.CI_upper",
                          "gt_ConvNextT_raw.mean1","gt_ConvNextT_raw.mean2","gt_ConvNextT_raw.CI_lower", "gt_ConvNextT_raw.CI_upper",
                          "species", "species"), nrow(all_overlap_coeffs) / 2), 
         key = rep(1:(nrow(all_overlap_coeffs) / 2), each = 2)) %>%
  select(-key)


data.frame(split(all_overlap_coeffs$gt_ResNet50_raw.CI, cut(1:nrow(all_overlap_coeffs), 2, labels = LETTERS[1:2])))
