library(Distance)
library(tidyverse)
library(lubridate)

CLNU.dat<-read.csv("CLNU_data_summary.csv")
distance.dat2020 <-read.csv ("CLNUdist_unmarked_removed_repeats.csv")
distance.dat2019<-read.csv("CLNUdist2.csv")

str(distance.dat2020)
str(distance.dat2019)
distance.dat2020$date<-mdy(distance.dat2020$Date)
distance.dat2019$date<-mdy(distance.dat2019$Date)

distance.dat2019$odistance<-distance.dat2019$distance
distance.dat2019$distance<-distance.dat2019$distance/1000
distance.dat2020$odistance<-distance.dat2020$distance
distance.dat2020$distance<-distance.dat2020$distance/1000


### To extract distance data for just visual obs
dist2020vis <- distance.dat2020[distance.dat2020$detection_type=="V", ]
dist2020.samp <- unique(distance.dat2020$Sample.Label)
miss.lab <- dist2020[!is.element(el=dist2020, set=dist2020.samp)]
str(miss.lab)

miss.data <- dist2020[is.element(dist2020$Sample.Label, miss.lab), ]

length(miss.data$Sample.Label)

miss.data <- miss.data[!duplicated(miss.data$Sample.Label), ]

miss.data$distance <- rep(NA, length(miss.lab))
miss.data$species <- rep("NA", length(miss.lab))
miss.data$visit <- rep(NA, length(miss.lab))

dist2020 <- rbind(dist2020, miss.data)

dist2020 <- dist2020[order(dist2020$Sample.Label), ]
dist2020<-distance.dat2020
### Format the two data sets to combine them
dist2020$Month<-dist2020$survey_period

dist2020<- subset(dist2020vis, select = c(Year, Forest_type, Observers, Sample.Label,
                                       Month, Noise_Level, Effort, Area,
                                       Region.Label,distance,date, Time_Detected))
dist2019<-subset(distance.dat2019, select = c(Year, Forest_type, Observers, Sample.Label,
                                       Month, Noise_Level, Effort, Area,
                                      Region.Label,distance,date,Time_Detected))


dist2019$oMonth[dist2019$Month=="JUL_AUG"] <- "8"                  
dist2019$oMonth[dist2019$Month=="SEP"] <- "9" 
dist2019$oMonth[dist2019$Month=="OCT"] <- "10" 
dist2019$Month<-as.factor(dist2019$oMonth)

dist2020$oMonth[dist2020$Month=="July"] <- "8" 
dist2020$oMonth[dist2020$Month=="August"] <- "8"                  
dist2020$oMonth[dist2020$Month=="September"] <- "9" 
dist2020$oMonth[dist2020$Month=="October"] <- "10" 
dist2020$Month<-as.factor(dist2019$oMonth)
dist2020$Month<- dist2020$oMonth

dist2019<-subset(dist2019, select = c(Year, Forest_type, Observers, Sample.Label,
                                              Month, Noise_Level, Effort, Area,
                                              Region.Label,distance,date,
                                      Time_Detected))
dist2020<-subset(dist2020, select = c(Year, Forest_type, Observers, Sample.Label,
                                      Month, Noise_Level, Effort, Area,
                                      Region.Label,distance,date,
                                      Time_Detected))

str(dist2020)
str(dist2019)
dist.tot<-rbind(dist2019,dist2020)

str(dist.tot)

write.csv(dist.tot,
          "C:\\Users\\Owner\\Documents\\UC Denver\\dist.tot.csv")

hist(dist.tot$distance)

#### Reduce time period of surveys to 10 and 5 mins
dist_10mins<- subset(dist.tot, Time_Detected <3)
dist_5mins<- subset(dist.tot, Time_Detected <2)

write.csv(dist_10mins,
          "C:\\Users\\Owner\\Documents\\UC Denver\\dist_10mins.csv")
write.csv(dist_5mins,
          "C:\\Users\\Owner\\Documents\\UC Denver\\dist_5mins.csv")


###Distance Analysis
dist.tot<-read.csv("dist.tot.csv")
hist(dist.tot$distance, breaks = 50)

CDS1<-ds(dist.tot, transect = "point", key = "unif", adjustment = "cos",
         truncation = .2)
plot(CDS1, pdf = TRUE)
summary(CDS1)

CDS2<-ds(dist.tot, transect = "point", key = "hn", adjustment = "herm",
         truncation = .2)
plot(CDS2, pdf = TRUE)
summary(CDS2)

CDS3<-ds(dist.tot, transect = "point", key = "hr", adjustment = "poly",
         truncation = .2)
plot(CDS3, pdf = TRUE)
summary(CDS3)

MCDStable<-summarize_ds_models(CDS1,CDS2,CDS3)

MCDStable

unique(dist.tot$Sample.Label)


###Distance analysis with bins
hist(dist.tot$distance, xlim = c(0.0,0.2), breaks = 50)

dist.bins1<- c(0.0,0.03,0.07,0.15)
dist.bins2<- c(0.0,0.01,0.03,0.07,0.15)
dist.bins3<- c(0.0,0.01,0.07,0.15,0.2)
dist.bins4<- c(0.0,0.01,0.08,0.2)
dist.bins4<- c(0.0,0.01,0.05,0.08,0.15,0.2)

CDS1<-ds(dist.tot, transect = "point", key = "unif", adjustment = "cos",
         truncation = .2, cutpoints = dist.bins4)
plot(CDS1, pdf = TRUE)
summary(CDS1)

CDS2<-ds(dist.tot, transect = "point", key = "hn", adjustment = "herm",
         truncation = .2, cutpoints = dist.bins4)
plot(CDS2, pdf = TRUE)
summary(CDS2)

CDS3<-ds(dist.tot, transect = "point", key = "hr", adjustment = "poly",
         truncation = .2, cutpoints = dist.bins4)
plot(CDS3, pdf = TRUE)
summary(CDS3)

MCDStable<-summarize_ds_models(CDS1,CDS2,CDS3)

MCDStable


#### 0.3 km truncation
CDS1<-ds(dist.tot, transect = "point", key = "unif", adjustment = "cos", truncation = .3)
plot(CDS1, pdf = TRUE)
summary(CDS1)

CDS2<-ds(dist.tot, transect = "point", key = "hn", adjustment = "herm", truncation = .3)
plot(CDS2, pdf = TRUE)
summary(CDS2)

CDS3<-ds(dist.tot, transect = "point", key = "hr", adjustment = "poly", truncation = .3)
plot(CDS3, pdf = TRUE)
summary(CDS3)

MCDStable<-summarize_ds_models(CDS1,CDS2,CDS3)

MCDStable


### shorter truncation

CDS.trunc1<-ds(dist.tot, transect = "point", key = "unif", adjustment = "cos",
         truncation = .06)
plot(CDS.trunc1, pdf = TRUE)
summary(CDS.trunc1)

CDS.trunc2<-ds(dist.tot, transect = "point", key = "hn", adjustment = "herm",
         truncation = .06)
plot(CDS.trunc2, pdf = TRUE)
summary(CDS.trunc2)

CDS.trunc3<-ds(dist.tot, transect = "point", key = "hr", adjustment = "poly",
         truncation = .06)
plot(CDS.trunc3, pdf = TRUE)
summary(CDS.trunc3)

MCDStable<-summarize_ds_models(CDS.trunc1,CDS.trunc2,CDS.trunc3)

MCDStable

###Shorter truncation with bins
###These models consistiently estimate density around 400-500 birds per unit, much higher than
### we can reasonably expect
### Additionally, severe truncation reduces the certainty around estimates due to a smaller
### sample size and a higher area required to predict over.  These estimates are far too
### imprecise to be usable
short.bins<- c(0.0,0.01, 0.04,0.07,0.08)

CDS.trunc1<-ds(dist.tot, transect = "point", key = "unif", adjustment = "cos",
               truncation = .08, cutpoints = short.bins)
plot(CDS.trunc1, pdf = TRUE)
summary(CDS.trunc1)

CDS.trunc2<-ds(dist.tot, transect = "point", key = "hn", adjustment = "herm",
               truncation = .08, cutpoints = short.bins)
plot(CDS.trunc2, pdf = TRUE)
summary(CDS.trunc2)

CDS.trunc3<-ds(dist.tot, transect = "point", key = "hr", adjustment = "poly",
               truncation = .08, cutpoints = short.bins)
plot(CDS.trunc3, pdf = TRUE)
summary(CDS.trunc3)

MCDStable<-summarize_ds_models(CDS.trunc1,CDS.trunc2,CDS.trunc3)

MCDStable

