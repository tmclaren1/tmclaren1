library(unmarked)
library(tidyverse)
library(lubridate)

CLNUdist_revised2<-read.csv("dist_5mins_revised.csv")
CLNUdist_revised2<- dist_5mins
str(CLNUdist_revised2)

CLNUdist_revised2$Date<-mdy(CLNUdist_revised2$date) 
CLNUdist_revised2$survey<-paste(CLNUdist_revised2$Sample.Label,CLNUdist_revised2$Date,sep="-")#add SurveyID column


five.min.bins<- c(0.0,0.17,0.25,0.35)#distance bins

area<-5*(3.14159*0.35^2)#area surveyed per transect


test<-formatDistData(CLNUdist_revised2, distCol = "distance",
                     transectNameCol = "survey", dist.breaks = five.min.bins)
str(test)
test

x <- data.frame("Site" = c("Avalanche Peak",
                           "Baronette",  
                           "Bear Jam",
                           "Canyon", 
                           "Confluence",
                           "DeLacy North",
                           "DeLacy South",
                           "Dunraven",
                           "Mammoth",
                           "Trout Lake",
                           "West Thumb"),
                "Forest_type" = c("PIAL",
                                  "PIEN", 
                                  "PSME",
                                  "PICO",
                                  "PIFL", 
                                  "PICO",
                                  "PIAL",
                                  "PIAL",
                                  "PIFL",
                                  "PSME",
                                  "PIEN"))

yearly.covs <- data.frame("Year" = c(1,2,
                                     2,
                                     1,2,
                                     1,2,
                                     2,
                                     1,2,
                                     1,2,
                                     1,2,
                                     1,2,
                                     1,2,
                                     2))



##separate out survey periods
clnu.2019<-CLNUdist_revised2%>%####use filter to create a separate matrix
  filter(Year=="2019")        ######for each visit n=7
clnu.2020<-CLNUdist_revised2%>%
  filter(Year=="2020")
clnu.2019
clnu.2020

clnu.19<-formatDistData(clnu.2019, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = five.min.bins)
clnu.19
clnu.20<-formatDistData(clnu.2020, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = five.min.bins)
clnu.20


dist.mat<-cbind(clnu.19,clnu.20)

###Set up unmarked frame

open.umf<- unmarkedFrameGDS(y= dist.mat,siteCovs = data.frame (x,area), numPrimary = 2,
                            yearlySiteCovs = yearly.covs,
                            dist.breaks=five.min.bins, survey = "point",unitsIn = "km")
###This is still a work in progress, formatting this table needs some work
summary(open.umf)

open.dist1nb<-gdistsamp(lambdaformula = ~1,phiformula = ~1,pformula= ~1, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
open.dist2nb<-gdistsamp(lambdaformula = ~1,phiformula = ~Year,pformula= ~1, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")

summary(open.dist1nb)
