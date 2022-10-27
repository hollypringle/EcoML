###LOAD PACKAGES

library(unmarked)
library(here)
library(dplyr)
library(data.table)
library(chron)
library(rlist)
library(ggplot2)
library(tibble)
library(jagsUI)
library(wiqid)
library(psych)
library(tidyr)
library(arsenal)
library(abind)
library(unmarked)
library(lubridate)
library(ggcorrplot)
library(cor_plot)

ResNet50_1perc<-read.csv("kenya_ecoml_labels_ResNet50_1perc.csv")
###BUILDING 4-DAY MATRICES FOR NATIVE MAMMALS
names(ResNet50_1perc)
range(ResNet50_1perc$date)
ResNet50_1perc <- rename(ResNet50_1perc, Species = pred1_label) #to match function below
ResNet50_1perc <- rename(ResNet50_1perc, DateTime = datetime) #to match function below
ResNet50_1perc <- rename(ResNet50_1perc, Site = location) #to match function below

ResNet50_1perc$DateTime <- as.POSIXct(ResNet50_1perc$DateTime, "%Y-%m-%d %H:%m:%s") # making sure date is Date
ResNet50_1perc$DateTime <- as.Date(ResNet50_1perc$DateTime, "%Y-%m-%d %H:%m:%s") # making sure date is Date

#Subsetting survey effort to same dates as test set #### 2018-11-10 to 2018-11-29 
DATE1 <- as.Date("2018-11-10")
DATE2 <- as.Date("2018-11-29")

all_cams <- read.csv("C:/Users/Holly/Documents/Biome_Health_Project/Data/all_sites_effort_occupancy_v2_2018.csv")
all_cams$Date<-as.Date(all_cams$Date, format="%Y.%m.%d")
myfunc <- function(x,y){all_cams[all_cams$Date>= x & all_cams$Date<= y,]}
all_cams <- myfunc(DATE1,DATE2)  
nrow(all_cams)
head(all_cams)
tail(all_cams)

all_cams$Date <- as.Date(all_cams$Date, "%Y.%m.%d") # making sure date is Date
str(all_cams)
all_cams<-all_cams[-(18:20), , drop = FALSE] #drop last 3 days
startDate <- as.Date("2018-11-10","%Y-%m-%d")
endDate <- as.Date("2018-11-26","%Y-%m-%d")

#calcOcc function:
calcOcc <-
  function(species, # species name - in dataframe - that the function is to be run for
           d = d, # dataframe with species, site, and each date it was seen at that site - must have a columns called Species, Site and DateTime
           all_cams = all_cams, # matrix with all the survey dates, 1s for dates when a camera was working/deployed and NAs for when it wasn't
           startDate = startDate,#start date in date format
           endDate = endDate) {
    # Make a vector of breaks
    brks <-seq(startDate, endDate, by = "day")   #makes a sequence of all the days from start to end
    
    # Create an empty matrix of dim sites x time periods
    occ <-matrix(0, ncol = length(unique(d$Site)), nrow = length(brks))
    colnames(occ) <- sort(unique(d$Site))
    rownames(occ) <- strftime(brks, format = "%Y-%m-%d")
    
    for (s in unique(d$Site)) {
      #this loops through each site and inserts 1's on days which there were captures
      seen <- NA
      captures <-na.omit(d$DateTime[d$Species == species & d$Site == s])
      # Were animals seen at the site
      seen <- which(brks %in% captures)
      # If the species was seen, occ = 1
      col_i <- which(colnames(occ) == s)
      occ[seen, col_i] <- 1
    }
    
    occ <- occ * all_cams[, 2:ncol(all_cams)]
    print(paste0(species, " done!"))
    species_name <- gsub(" ", "", species)
    row.names(occ) <- brks
    write.csv(occ, paste0("1d_matrix_", species_name, ".csv"))
    return(occ)
    
    
  }


#Creating single day matrices:
setwd("~/Biome_Health_Project/EcoML_paper/impact of training_set_size/matrices_ResNet50_1perc") #change direcory so matrices stored in folder

# applying function to each species (label)
lapply(
  X = unique(ResNet50_1perc$Species),
  FUN = calcOcc,
  d = ResNet50_1perc,
  all_cams=all_cams,
  startDate = startDate,
  endDate = endDate) # this will save CSVs of spp matrices in the working directory

setwd("~/Biome_Health_Project/EcoML_paper/impact of training_set_size") #change back


#Reading in all CSVs with spp presence absence

filenames <- list.files("~/Biome_Health_Project/EcoML_paper/impact of training_set_size/matrices_ResNet50_1perc", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
head(ldf)
label <- basename(filenames)
label <- sub(pattern=".csv", replace="",label)
label <- sub(pattern="1d_matrix_", replace="",label)
names(ldf) <- label
new_list <- lapply(ldf, function(x) x%>% select(-X)) # eliminating col "X" with dates before applying timestepper func
new_list[[1]]

#Aggregate days into survey occasions. Timestepper - creates matrices of a given timestep, can choose to include or exclude NAs.
timestepper <- function(occ_in, timestep, na_mode = "include") {
  if (na_mode == "include") {
    occ_in[is.na(occ_in)] <-0   #replacing NAs with 0s if we want to include them in analysis.
  }
  
  if (timestep > nrow(occ_in) / 2) {
    print(paste(
      "Time step is too large! Please reduce to",
      nrow(occ_in) / 2 ,
      "or less."
    ))
  } else {
    start <- seq(1, nrow(occ_in), by = timestep)
    end <- seq(timestep, nrow(occ_in), by = timestep)
    
    if (length(start) > length(end)) {
      start <- start[-length(start)]
    }
    
    timesteps <- matrix(nrow = length(start), ncol = ncol(occ_in))
    colnames(timesteps) <- colnames(occ_in)
    rownames(timesteps) <-
      paste(rownames(occ_in)[start], rownames(occ_in)[end], sep = ":")
    
    for (i in 1:length(start)) {
      timestep_out <- colSums(occ_in[start[i]:end[i], ])
      timesteps[i, ] <- timestep_out
      timesteps[timesteps > 0] <- 1
    }
    
    timesteps <- t(timesteps)
    return(timesteps)
    
  }
  
}

#Applying timestepper function to aggregate into 4-day occasions:
matrix_1d <- lapply(X = new_list,
                    FUN = timestepper,
                    timestep = 1,
                    na_mode = "exclude")  # must be exclude otherwise will consider days when CT wasn't working as actual survey days
matrix_1d[[1]]
names(matrix_1d) <- label # adding species names

matrix_native_mammals_1d <- matrix_1d[c( #no aardvark
                                        1, #baboon
                                         #bateared_fox
                                        2, #buffalo
                                         #bushbuck
                                         #bustard
                                        #3, #cattle
                                         #cheetah
                                         #dikdik
                                        #domestic_dog
                                        4,#eland
                                        5,#elephant
                                        #6 #francolin
                                        7,#gazelle_grants
                                        8,#gazelle_thomsons
                                        #genet
                                        9,#giraffe
                                        #10,#goose
                                        ##guineafowl
                                        11,#hare
                                        12, #hartebeest_cokes
                                        ##heron_stork_crane_ibis_hamerkop
                                        13, #hippopotamus
                                        14, #hyena_aardwolf
                                        15, #impala
                                        16, #jackal
                                        ##lapwing_plover_thickknee
                                        #leopard
                                        #lion
                                        17, #mongoose
                                        18, #oribi
                                        # #ostrich
                                        #19, #other
                                        #20, #other_nonterrestrial_bird
                                        #21, #other_raptor
                                        22, #reedbuck 
                                        # #other_rodents
                                         #porcupine
                                        #serval
                                        #23 #shoat
                                        24, #topi
                                        25, #vervet_monkey
                                        26, #warthog
                                        #waterbuck
                                        27, #wildebeest
                                        28 #zebra
                                        #zorilla
)] # keeping only native mammals
names(matrix_native_mammals_1d)
write.csv(matrix_native_mammals_1d, "matrix_native_mammals_1d_ResNet50_1perc.csv")

## just checking
names(matrix_native_mammals_1d) # must have only spp used in analysis
zebra <- matrix_native_mammals_1d[[21]]
sum(zebra, na.rm = T) # number of detections for zebra considering the 4-day survey occasion (i.e. max 1 detection per 4 days)

baboon <- matrix_native_mammals_1d[[1]]
sum(baboon, na.rm = T) # number of detections for warthog considering the 4-day survey occasion (i.e. max 1 detection per 4 days)

#####MODELS####

##code below will create all-0 matrices for hypothetical spp never detected during the survey (augmented species)
##this is part of the data-augmentation approach to estimate spp richness via multi-species occupancy modelling (Bayesian approach)
##if running single-species occupancy models there is no need to create augmented spp
##75 mammal species expected in broad study area according to IUCN. This includes mammals except rodents (except porcupine), bats, shrews etc. 
##75 expected- 40 observed (excluding birds)= 35
##haven't included birds as they are mostly grouped and not to species
#
Aug01 <- matrix_native_mammals_1d[[1]] # getting matrix for spp1 as base
Aug01[Aug01==1] <-0 # making all detection 0
sum(Aug01, na.rm=T )
Aug02 <- Aug01
sum(Aug02, na.rm=T)
Aug03 <- Aug01
Aug04 <- Aug01
Aug05 <- Aug01
Aug06 <- Aug01
Aug07 <- Aug01
Aug08 <- Aug01
Aug09 <- Aug01
Aug10 <- Aug01
Aug11 <- Aug01
Aug12 <- Aug01
Aug13 <- Aug01
Aug14 <- Aug01
Aug15 <- Aug01
Aug16 <- Aug01
Aug17 <- Aug01
Aug18 <- Aug01
Aug19 <- Aug01
Aug20 <- Aug01
Aug21 <- Aug01
Aug22 <- Aug01
Aug23 <- Aug01
Aug24 <- Aug01
Aug25 <- Aug01
Aug26 <- Aug01
Aug27 <- Aug01
Aug28 <- Aug01
Aug29 <- Aug01
Aug30 <- Aug01
Aug31 <- Aug01
Aug32 <- Aug01
Aug33 <- Aug01
Aug34 <- Aug01
Aug35 <- Aug01
sum(Aug10, na.rm=T) # checking
#
## adding augmented spp to the list of native spp matrices
matrix_native_mammals_1d$Aug01 <- Aug01
matrix_native_mammals_1d$Aug02 <- Aug02
matrix_native_mammals_1d$Aug03 <- Aug03
matrix_native_mammals_1d$Aug04 <- Aug04
matrix_native_mammals_1d$Aug05 <- Aug05
matrix_native_mammals_1d$Aug06 <- Aug06
matrix_native_mammals_1d$Aug07 <- Aug07
matrix_native_mammals_1d$Aug08 <- Aug08
matrix_native_mammals_1d$Aug09 <- Aug09
matrix_native_mammals_1d$Aug10 <- Aug10
matrix_native_mammals_1d$Aug11 <- Aug11
matrix_native_mammals_1d$Aug12 <- Aug12
matrix_native_mammals_1d$Aug13 <- Aug13
matrix_native_mammals_1d$Aug14 <- Aug14
matrix_native_mammals_1d$Aug15 <- Aug15
matrix_native_mammals_1d$Aug16 <- Aug16
matrix_native_mammals_1d$Aug17 <- Aug17
matrix_native_mammals_1d$Aug18 <- Aug18
matrix_native_mammals_1d$Aug19 <- Aug19
matrix_native_mammals_1d$Aug20 <- Aug20
matrix_native_mammals_1d$Aug21 <- Aug21
matrix_native_mammals_1d$Aug22 <- Aug22
matrix_native_mammals_1d$Aug23 <- Aug23
matrix_native_mammals_1d$Aug24 <- Aug24
matrix_native_mammals_1d$Aug25 <- Aug25
matrix_native_mammals_1d$Aug26 <- Aug26
matrix_native_mammals_1d$Aug27 <- Aug27
matrix_native_mammals_1d$Aug28 <- Aug28
matrix_native_mammals_1d$Aug29 <- Aug29
matrix_native_mammals_1d$Aug30 <- Aug30
matrix_native_mammals_1d$Aug31 <- Aug31
matrix_native_mammals_1d$Aug32 <- Aug32
matrix_native_mammals_1d$Aug33 <- Aug33
matrix_native_mammals_1d$Aug34 <- Aug34
matrix_native_mammals_1d$Aug35 <- Aug35

#
names(matrix_native_mammals_1d)
sum(matrix_native_mammals_1d[[50]], na.rm=T) # sum =0; ok as it is an aug spp
#

#####Now onto building the occupancy model####

matrix_analysis_1d.noNA<-lapply(matrix_native_mammals_1d, function(x) x[rowSums(is.na(x)) != ncol(x),]) #remove rows with all NA

#aardvark<-matrix_native_mammals_1d[[1]]
baboon_noNA<- matrix_analysis_1d.noNA[[1]]
names(matrix_analysis_1d.noNA) #check
names(matrix_native_mammals_1d) #check

covs<-read.csv("C:/Users/Holly/Documents/Biome_Health_Project/EcoML_paper/covariates_2018_livestockrates_v3.csv")
covs_select<-covs%>%  select(CT_site,propopen500m, waterdist_short, humdist_short,tree_shrub_density,Avg_grass)
str(covs_select)

#####Check order of sites
baboon_noNadf<-as.data.frame(baboon_noNA)
baboon_noNadf<-setDT(baboon_noNadf, keep.rownames = TRUE[])

covs_select2 <- covs_select[match(baboon_noNadf$rn,covs_select$CT_site),] #matching order and sites included of covs and matrices - sites with 11 or more NAs in matrices will be removed from both dfs

head(baboon_noNadf)
head(covs_select2) #match- yes
#
tail(baboon_noNadf) #match - yes
tail(covs_select2) #match yes
#
baboon_noNadf[40:50,]
covs_select2[40:50,] #match- yes

baboon_noNadf[80:90,]
covs_select2[80:90,] #match- yes

#
unique(covs_select2$CT_site)
unique(baboon_noNadf$rn)

cor_plot <- function(data, method=c("pearson", "kendall", "spearman"),
                     sort=FALSE,
                     axis_text_size=12,
                     number_text_size=3,
                     legend=FALSE){
  method <- match.arg(method)
  index <- sapply(data, is.numeric)
  qdata <- data[index]
  qdata <- na.omit(qdata)
  # bind global variables to keep check from warning
  r <- stats::cor(qdata, method=method)
  p <- ggcorrplot(r,
                  hc.order = sort,
                  colors = c("blue", "white", "red"),
                  type = "lower",
                  lab = TRUE,
                  lab_size=number_text_size,
                  show.legend=legend)
  n <- format(nrow(qdata), big.mark=",")
  if (method == "pearson"){
    subtitle <- paste0("Pearson correlations (n = ",n, ")")
  }
  if (method == "spearman"){
    subtitle <- paste0("Spearman rank order correlations (n = ",n, ")")
  }
  if (method == "kendall"){
    subtitle <- paste0("Kendall rank order correlations (n = ",n, ")")
  }
  p <- p + labs(title = "Correlation Matrix",
                subtitle = subtitle) +
    theme(axis.text.x=element_text(size=axis_text_size),
          axis.text.y=element_text(size=axis_text_size),
          plot.subtitle = element_text(size=8,
                                       face="plain"))
  return(p)
}

covs_select_noCT<-covs_select2
covs_select_noCT$CT_site<-NULL
covs_numeric<- as.data.frame(scale(covs_select_noCT))
corplot_covs<-covs_numeric%>% cor_plot() #all v low correlation???

#
X <- abind(matrix_analysis_1d.noNA, along=3)
testaug<-sum(list.extract(matrix_analysis_1d.noNA, 50), na.rm=T) #checking augmented species
n <- dim(X)[3]-35 # number of observed spp (total n of species minus the number of augmented species)
nzeroes <- dim(X)[3]-21 # number of augmented (all 0) species in the data set
J <- dim(X)[1] # number of CT sites
Ktemp <- X[,,1]
Ktemp[Ktemp==0] <- 1  # 1 indicates CT was working, therefore a valid survey occasion
Ktemp[is.na(Ktemp)] <- 0 # 0 indicates CT was NOT working, therefore not a valid survey occasion and represented as NA in the species presence-abs matrix
Knamed <- rowSums(Ktemp) # sum of rows (i.e. of 1s) indicate numberr of valid survey occasions at each site
K <- unname(Knamed) # number of surveys at each CT site #all should have at least 2
Knamed-K # fine, all 0
K <- as.integer(K)
unique(K)
length(K) # must match number of CT sites - ok




####Standardize covariates and onto the model!

##standardize is the same as scale
names(covs_select2)
head(covs_select2)
nrow(covs_select2)

tree_shrub_density <- standardize(covs_select2$tree_shrub_density)
Avg_grass <- standardize(covs_select2$Avg_grass)
propopen <- standardize(covs_select2$propopen500m)
waterdist <- standardize(covs_select2$waterdist_short)
humdist <- standardize(covs_select2$humdist_short)


####FIVE MINUTE INDIVIDUAL LIVESTOCK RATES WITH QUADRATIC EFFECT####
## Step 3 - Bundle and summarize data

str(sp.data.event <- list(n = n, nzeroes = nzeroes, J = J, K = K, X = X,
                          #                               s=Size, # size categories
                          tree_shrub_density = tree_shrub_density, 
                          Avg_grass = Avg_grass, propopen = propopen,waterdist=waterdist, humdist=humdist))
#
## Step 4 - Defining initial values for the MCMC
#
wst <- rep(1, n+nzeroes)                   # Simply set everybody at occurring
zst <- array(1, dim = c(J, n+nzeroes)) # ditto
### wst and zst are as suggested by Kery & Royle 2015; this is diff from Zipkin et al 2010
#
sp.inits <- function() {
  omegaGuess = runif(1, n/(n+nzeroes), 1)
  psi.meanGuess = runif(1, .25,1)
  list(omega=omegaGuess, Z = zst, w = wst, alpha0=rnorm(n = n+nzeroes), 
       alphaOpen = rnorm(n = n+nzeroes), 
       alphaWater = rnorm(n = n+nzeroes), 
       alphaHum= rnorm(n = n+nzeroes), 
       beta0=rnorm(n = n+nzeroes), 
       betaShrub = rnorm(n = n+nzeroes), betaGrass = rnorm(n = n+nzeroes))
}
#
## parameters to monitor
params1 <- c("omega", 
             "mu.alpha0","mu.alphaOpen", "mu.alphaWater",
             "mu.alphaHum", "mu.beta0",
             "mu.betaGrass", "mu.betaShrub", "alpha0",
             "alphaOpen", "alphaWater",
             "alphaHum", "beta0",
             "betaGrass", "betaShrub", 
             "Nocc.fs","PropOcc.fs",
             "Nsite", 
             "Ntotal", "Omega","c.hat.sp", "c.hat", "bpv.sp", "bpv", 
             "spp.Pvalue", "overall.Pvalue"
)


## MCMC settings
ni <- 150000   ;   nt <- 10   ;   nb <- 50000   ;   nc <- 3
#
## Run JAGS, check convergence and summarize posteriors
#install.packages("jagsUI")
library(jagsUI)
#
out_EcoML_ResNet50_1perc<- jags(sp.data.event, sp.inits, params1, "Kenya_MultiSppModel_EcoML", 
                           n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
#
out_EcoML_ResNet50_1perc.summary<-print(out_EcoML_ResNet50_1perc, dig = 3)
out_EcoML_ResNet50_1perc.summary.df<-as.data.frame(out_EcoML_ResNet50_1perc$summary)
head(out_EcoML_ResNet50_1perc.summary.df)
highrhat.event<-subset(out_EcoML_ResNet50_1perc.summary.df, out_EcoML_ResNet50_1perc.summary.df$Rhat>1.05)
overlapzero.event<-subset(out_EcoML_ResNet50_1perc.summary.df, out_EcoML_ResNet50_1perc.summary.df$overlap0==0)
write.csv(out_EcoML_ResNet50_1perc.summary.df, "out_EcoML_ResNet50_1perc.summary.df.csv")



##Plotting
library(envnames)
library(qdapRegex)
library(ggplot2)
matrixnames<-as.data.frame(names(matrix_analysis_1d.noNA))
matrixnamesdf<-setDT(matrixnames, keep.rownames = TRUE[])
colnames(matrixnamesdf)[1]<-"species_no"
colnames(matrixnamesdf)[2]<-"species_name"
matrixnamesdf<-edit(matrixnamesdf) #add edited column for species names which look nicer
write.csv(matrixnamesdf, "matrixnames.csv")
matrixnamesdf<-as.data.frame(read.csv("matrixnames.csv"))
matrixnamesdf$species_no<-as.character(matrixnamesdf$species_no)

out_EcoML_gt.summary.DT<-out_EcoML_gt.summary.df
out_EcoML_gt.summary.DT<-setDT(out_EcoML_gt.summary.DT, keep.rownames = TRUE[])

out_EcoML_gt.summary.DT$species_no<-as.character(rm_between(out_EcoML_gt.summary.DT$rn,"[","]",extract=TRUE))
tail(out_EcoML_gt.summary.DT)
head(matrixnamesdf)
write.csv(matrixnamesdf, "matrixnames_v1.csv")
out_EcoML_gt.summary.DT <- merge(out_EcoML_gt.summary.DT,matrixnamesdf, by="species_no" )
colnames(matrixnamesdf)[2]<-"old_species_name"
write.csv(out_EcoML_gt.summary.DT, "out_EcoML_gt.summary.df_withnames.csv")

####SIGNIFICANT SPECIES####
# predicting effect of agric for spp in which CRI doesn't overlap 0 #
# first, finding spp
head(overlapzero.event)
overlapzero_spp <- overlapzero.event[3:31,]

# predicting
pred.humdist<- seq(from = min(humdist), to = max(humdist),length.out=100) # scaled values of prop Agric to plug into the prediction formula
str(tmp <- (out_EcoML_gt$sims.list))
nsamp <- length (tmp[[1]]) # number of MCM samplings
spp.predhumdist <- array(NA,dim=c(100, nsamp, 2)) #3rd dimension is for the spp coeefs, only ones not overlapping 0
str(spp.predhumdist)
for(i in 1:nsamp){
  spp.predhumdist[,i,1] <- plogis(tmp$alpha0[i,9] + tmp$alphaHum[i,9] * pred.humdist) # interaction between Agric and BZ for spp 1
  spp.predhumdist[,i,2] <- plogis(tmp$alpha0[i,32] + tmp$alphaHum[i,32] * pred.humdist) # agric and NP fo spp 2 
}

dimnames(spp.predhumdist)[[3]] <- c("Jackal", "Zebra")
spp.predhumdist.mean <- apply(spp.predhumdist, c(1,3), mean)  # getting the posterior mean of the predicted value 

# getting 95% credible interval of predictions
cri.spp.predhumdist <- apply(spp.predhumdist, c(1,3), function (x) quantile (x, probs = c(0.025, 0.975)))
lower.spp.predhumdist <- cri.spp.predhumdist[1,,] # just spliting array into DF
upper.spp.predhumdist <- cri.spp.predhumdist[2,,] # ditto
full.spp.predhumdist <- as.data.frame(cbind(spp.predhumdist.mean, lower.spp.predhumdist, upper.spp.predhumdist))
head(full.spp.predhumdist)
names(full.spp.predhumdist) <- c("Jackal", "Zebra",
                                 "LCI_Jackal", "LCI_Zebra","UCI_Jackal",
                                 "UCI_Zebra")

library(reshape2)
tmp3 <- melt(full.spp.predhumdist) # will stack all collumns and create a new col with variable name
tail(tmp3,20)  #checking
mean <- tmp3[1:200 ,]
LCI <- tmp3[201:400,]
UCI <- tmp3[401:600,]
tail(UCI)

new.spp.human.pred <- cbind(mean,LCI,UCI)
head(new.spp.human.pred)
names(new.spp.human.pred) <- c("Species","mean","LowerVar","LCI", "UpperVar","UCI")


library(dplyr)
library(tidyr)

range(covs_select$humdist_short)

real.humdist.2<- as.data.frame(seq(from = 94.89728, to = 6600.55776,length.out=100)) 
real.humdist.2 <- rbind(real.humdist.2, real.humdist.2) # stacking it 7 times, once per coeff predicted
names(real.humdist.2) <- "humDist"

new.spp.human.pred$humDist <- unlist(real.humdist.2)
head(new.spp.human.pred)
names(new.spp.human.pred) <- c("Species","mean","LowerVar","LCI", "UpperVar","UCI","humDist")

ggplot(new.spp.human.pred, aes(x=humDist, y=mean))+
  theme_bw()+ # white background (as opposed to the default grey)
  #geom_ribbon(aes(ymin=LCI, ymax=UCI, colour=Species),alpha=0.2)+
  geom_line(aes(colour=Species), size=1) + 
  #theme(legend.position=c(.8,.10))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(strip.text = element_text(size=11,lineheight=5.0),
        strip.background = element_rect(fill="gray90", colour="black"))+
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x  = element_text(size=10),
        axis.text.y  = element_text(size=10)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  #scale_fill_manual(values=c("yellow4", "forestgreen", "tomato3"))+
  #theme(legend.position="none")+
  ylab("Probability of occupancy")+
  xlab("Distance from humans")

ggplot(new.spp.human.pred, aes(x=humDist, y=mean))+
  facet_wrap(~Species, scales="free_y", nrow=2) +
  theme_bw()+ # white background (as opposed to the default grey)
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_ribbon(aes(ymin=LCI, ymax=UCI),alpha=0.2)+
  geom_line(aes(colour="black", linetype="solid"), size=1) + 
  scale_fill_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  scale_colour_manual(values=c("#1b9e77","#d95f02", "#7570b3"))+
  scale_linetype_manual(values=c("dotdash","solid", "longdash"))+
  scale_size_manual(values=c(1, 5))+
  coord_cartesian(ylim = c(0, 1))+
  #theme(legend.position=c(.8,.10))+
  theme(legend.title=element_blank())+
  theme(legend.text = element_text(size = 12))+
  theme(strip.text = element_text(size=11,lineheight=5.0),
        strip.background = element_rect(fill="gray90", colour="black"))+
  theme(axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        axis.text.x  = element_text(size=10),
        axis.text.y  = element_text(size=10)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))+
  theme(axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  #scale_fill_manual(values=c("yellow4", "forestgreen", "tomato3"))+
  theme(legend.position="none")+
  ylab("Probability of occupancy")+
  xlab("Distance from human infrastructure (m)")
