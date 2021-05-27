library(unmarked)
library(tidyverse)
library(lubridate)

CLNUdist_revised2<-read.csv("dist_5mins_revised.csv")
CLNUdist_revised2<- dist_5mins
str(CLNUdist_revised2)

CLNUdist_revised2$Date<-mdy(CLNUdist_revised2$date) 
CLNUdist_revised2$survey<-paste(CLNUdist_revised2$Sample.Label,CLNUdist_revised2$Date,sep="-")#add SurveyID column
unique(CLNUdist_revised2$survey)#look at the number of survey dates

#CLNUdist_revised2$odistance<-CLNUdist_revised2$distance# scale distance to km
#CLNUdist_revised2$distance<-CLNUdist_revised2$distance/1000

five.min.bins<- c(0.0,0.17,0.25,0.35)

area<-5*(3.14159*0.35^2)


test<-formatDistData(CLNUdist_revised2, distCol = "distance",
                     transectNameCol = "survey", dist.breaks = five.min.bins)


test<-formatDistData(CLNUdist_revised2, distCol = "distance",
                     transectNameCol = "survey", dist.breaks = bins2)
str(test)
test


x <- data.frame("Site" = c("Avalanche Peak","Avalanche Peak","Avalanche Peak", "Avalanche Peak","Avalanche Peak",
                           "Baronette", "Baronette", "Baronette", "Baronette", 
                           "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam",
                           "Canyon", "Canyon", "Canyon", "Canyon","Canyon", "Canyon", "Canyon",
                           "Confluence", "Confluence", "Confluence", "Confluence",
                           "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North",
                           "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South",
                           "Dunraven", "Dunraven", "Dunraven", "Dunraven", "Dunraven", "Dunraven",
                           "Mammoth", "Mammoth", "Mammoth", "Mammoth", "Mammoth", "Mammoth", "Mammoth",
                           "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake",
                           "West Thumb", "West Thumb", "West Thumb", "West Thumb"),
                "Forest_type" = c("PIAL","PIAL","PIAL", "PIAL","PIAL", 
                                  "PIEN", "PIEN", "PIEN", "PIEN", 
                                  "PSME", "PSME", "PSME", "PSME", "PSME", "PSME", "PSME",
                                  "PICO", "PICO", "PICO", "PICO", "PICO", "PICO", "PICO",
                                  "PIFL", "PIFL", "PIFL", "PIFL",
                                  "PICO", "PICO", "PICO", "PICO", "PICO", "PICO", "PICO",
                                  "PIAL", "PIAL", "PIAL", "PIAL", "PIAL", "PIAL", "PIAL",
                                  "PIAL", "PIAL", "PIAL", "PIAL", "PIAL", "PIAL",
                                  "PIFL", "PIFL", "PIFL", "PIFL", "PIFL", "PIFL", "PIFL",
                                  "PSME", "PSME", "PSME", "PSME", "PSME", "PSME", "PSME",
                                  "PIEN", "PIEN", "PIEN", "PIEN"),
                "Month" = c("8","9","8","8","9", 
                            "8", "8", "9", "10",
                            "8", "9", "10", "8", "8", "9",  "10",
                            "8", "9", "10", "8", "8", "9", "10",
                            "8", "8", "9", "10",
                            "8", "9", "10", "8", "8", "9", "10",
                            "8", "9", "10", "8", "8", "9", "10",
                            "8", "9", "8", "8", "9", "10",
                            "8", "9", "10", "8", "8", "9", "10", 
                            "8", "9", "10", "8", "8", "9", "10",
                            "8", "8", "9", "10" ),
                "Observers" = c("TM","TM","TM","TM","TM", 
                                "TM","TM","TM/DT", "WW", 
                                "TM/DT","WW","WW","TM","TM","TM/DT","TM/DT", 
                                "TM","WW","WW","TM/DT", "TM", "WW", "WW", 
                                "TM", "TM", "TM/DT", "TM/DT", 
                                "TM","WW", "TM","TM", "TM", "TM", "WW", 
                                "TM/DT","WW","WW","TM", "TM", "WW", "WW",  
                                "TM/DT","TM","TM", "TM", "DT", "TM/DT",  
                                "TM","TM","TM","TM", "TM","TM/DT","TM",  
                                "TM","WW","TM","TM", "TM","TM","WW",  
                                "TM", "TM", "WW", "TM"), 
                "Effort" = c("5","5","5","5","5", 
                             "4", "4", "4", "4", 
                             "7", "7", "7", "7", "7", "7", "7", 
                             "7", "7", "7", "7", "7", "7", "7", 
                             "4", "4", "4", "4", 
                             "7", "7", "7", "7", "7", "7", "7", 
                             "7", "7", "7", "7", "7", "7", "7",  
                             "6", "6", "6", "6", "6", "6", 
                             "7", "7", "7", "7", "7", "7", "7",  
                             "7", "7", "7", "7", "7", "7", "7",  
                             "4", "4", "4", "4" ),
                "Tot_Cones" = c(31,31,186,186,186,
                                872, 872, 872, 872, 
                                37, 37, 37, 172, 172, 172, 172,
                                49, 49, 49, 120, 120, 120, 120,
                                3243, 3243, 3243, 3243,
                                73, 73, 73, 119, 119, 119, 119,
                                151, 151, 151, 101, 101, 101, 101,
                                3, 3, 324, 324, 324, 324,
                                9, 9, 9, 775, 775, 775, 775,
                                155, 155, 155, 1979, 1979, 1979, 1979,
                                1189, 1189, 1189, 1189),
                "Year" = c(1,1,2,2,2,
                           2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           2,2,2,2),
               Tot_Cones0,
               Tot_Cones1,
               Tot_Cones2)

yearly.covs<-data.frame( "Year" = c(1,1,2,2,2,
                           2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           1,1,1,2,2,2,2,
                           2,2,2,2))

str(x)

Tot_Cones = c(31,31,186,186,186,
                872, 872, 872, 872, 
                37, 37, 37, 172, 172, 172, 172,
                49, 49, 49, 120, 120, 120, 120,
                3243, 3243, 3243, 3243,
                73, 73, 73, 119, 119, 119, 119,
                151, 151, 151, 101, 101, 101, 101,
                3, 3, 324, 324, 324, 324,
                9, 9, 9, 775, 775, 775, 775,
                155, 155, 155, 1979, 1979, 1979, 1979,
                1189, 1189, 1189, 1189)
Tot_Cones0<-scale(Tot_Cones, center = FALSE,scale = TRUE)
Tot_Cones1<-scale(Tot_Cones, center = TRUE,scale = TRUE)
Tot_Cones2<-scale(Tot_Cones, center = TRUE,scale = FALSE)
Tot_Cones1<-Tot_Cones
#x <- data.frame("Forest_type" = c("PIAL", "PIEN", "PSME", "PICO", "PIFL", "PICO", "PIAL",
#                                  "PIAL", "PIFL", "PSME", "PIEN"))

umftest<- unmarkedFrameDS(y= test,siteCovs = data.frame (x,area),
                          dist.breaks=five.min.bins, survey = "point",unitsIn = "km")
str(umftest)
###Models
hdstest<-distsamp(~Forest_type~Forest_type,umftest,keyfun = "halfnorm", output = "density",
                  unitsOut= "kmsq")
hdstest2<-distsamp(~1~Forest_type,umftest,keyfun = "halfnorm", output = "density",
                   unitsOut= "kmsq")
hdstest3<-distsamp(~1~Forest_type + Month,umftest,keyfun = "halfnorm", output = "density",
                   unitsOut= "kmsq")
hdstest4<-distsamp(~1~Forest_type * Month,umftest,keyfun = "halfnorm", output = "density",
                   unitsOut= "kmsq")
hdstest5<-distsamp(~Forest_type~Forest_type + Month,umftest,keyfun = "halfnorm", output = "density",
                   unitsOut= "kmsq")
hdstest6<-distsamp(~Forest_type~Forest_type * Month,umftest,keyfun = "halfnorm", output = "density",
                   unitsOut= "kmsq")
hdstest7<-distsamp(~Month~Forest_type,umftest,keyfun = "halfnorm", output = "density",
                   unitsOut= "kmsq")
hdstest
summary(hdstest)
hdstest2
summary(hdstest2)

hdstest3
hdstest4
hdstest5
hdstest6
hdstest7
#hdstest2<-gdistsamp(~Forest_type,~1,~Forest_type, umftest, keyfun = "hazard", output= "density",
#unitsOut = "density", mixture = "NB")

###Create fitstats as a function to return multiple bootstrap statistics
fitstats <- function(fm) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  resids <- residuals(fm)
  sse <- sum(resids^2)
  chisq <- sum((observed - expected)^2 / expected)
  freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
  out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
  return(out)
}
(pb<- parboot(hdstest7,statistic=fitstats, nsim=1000, report=5))
(c.hat<-pb@t0[2]/mean(pb@t.star[,2]))


###Model over-dispersion using Gdistsamp and NB regression
gtest<-formatDistData(CLNUdist_revised2, distCol = "distance",
                      transectNameCol = "survey", dist.breaks = bins2)

gtest<-formatDistData(CLNUdist_revised2, distCol = "distance",
                      transectNameCol = "survey", dist.breaks = five.min.bins)

gumftest<- unmarkedFrameGDS(y= gtest,siteCovs = data.frame (x,area), numPrimary = 1,
                            dist.breaks=five.min.bins, survey = "point",unitsIn = "km")

summary(gumftest)
##Gdist models

gdist1nb<-gdistsamp(lambdaformula = ~1,phiformula = ~1,pformula= ~1,
                    data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist2nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,
                    pformula= ~1, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist3nb<-gdistsamp(lambdaformula = ~Month,phiformula = ~1,pformula= ~1, 
                    data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist4nb<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,
                    pformula= ~1, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist5nb<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,
                    pformula= ~Forest_type, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist6nb<-gdistsamp(lambdaformula = ~Forest_type + Month ,phiformula = ~1,
                    pformula= ~Month, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist7nb<-gdistsamp(lambdaformula = ~Forest_type * Month,phiformula = ~1,
                    pformula= ~Month, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist8nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,
                    pformula= ~Month, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist9nb<-gdistsamp(lambdaformula = ~Forest_type +Tot_Cones1,phiformula = ~1,
                    pformula= ~Month, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist10nb<-gdistsamp(lambdaformula = ~Forest_type *Tot_Cones1,phiformula = ~1,
                     pformula= ~1, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist11nb<-gdistsamp(lambdaformula = ~Forest_type *Tot_Cones1,phiformula = ~1,
                     pformula= ~Month, data = gumftest,
                     output = "density", unitsOut = "kmsq", mixture= "NB")

gdist12nb<-gdistsamp(lambdaformula = ~Forest_type *Tot_Cones1,phiformula = ~1,
                     pformula= ~Forest_type, data = gumftest,
                     output = "density", unitsOut = "kmsq", mixture= "NB")

gdist13nb<-gdistsamp(lambdaformula = ~Forest_type + Tot_Cones1,phiformula = ~1,
                     pformula= ~Forest_type, data = gumftest,
                     output = "density", unitsOut = "kmsq", mixture= "NB",
                     starts = c(0,0,0,0,0,0,0,0,0,0,0,0))

gdist14nb<-gdistsamp(lambdaformula = ~Forest_type + Tot_Cones1,phiformula = ~1 ,
                     pformula= ~1, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist15nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1 ,
                     pformula= ~Observers, data = gumftest,
                     output = "density", unitsOut = "kmsq", mixture= "NB")

gdist16nb<-gdistsamp(lambdaformula = ~Forest_type +Month +Tot_Cones1,phiformula = ~1 ,
                     pformula= ~1, data = gumftest,
                     output = "density", unitsOut = "kmsq", mixture= "NB")


gdist17nb<-gdistsamp(lambdaformula = ~Forest_type +Month +Tot_Cones1,phiformula = ~1 ,
                     pformula= ~Forest_type, data = gumftest,
                     output = "density", unitsOut = "kmsq", mixture= "NB",
                     starts = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0))

gdist18nb<-gdistsamp(lambdaformula = ~Tot_Cones,phiformula = ~1,pformula= ~Forest_type,
                       data = gumftest,
                       output = "density", unitsOut = "kmsq", mixture= "NB",
                     starts = c(0,0.01,1,1,1,1,0,0))



gdist1pois<-gdistsamp(lambdaformula = ~1,phiformula = ~1,pformula= ~1, data = gumftest,
                      output = "density", unitsOut = "kmsq", mixture = "P")
gdist2pois<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~1, data = gumftest,
                      output = "density", unitsOut = "kmsq", mixture= "P")
gdist3pois<-gdistsamp(lambdaformula = ~Month,phiformula = ~1,pformula= ~1, data = gumftest,
                      output = "density", unitsOut = "kmsq", mixture= "P")
gdist4pois<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,pformula= ~1, data = gumftest,
                      output = "density", unitsOut = "kmsq", mixture= "P")
gdist5pois<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,pformula= ~Forest_type, data = gumftest,
                      output = "density", unitsOut = "kmsq", mixture= "P")
#gdist6pois<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,pformula= ~Month, data = gumftest,
#                   output = "density", unitsOut = "kmsq", mixture= "P")


gdist1nb
gdist2nb
gdist3nb
gdist4nb
gdist5nb
gdist6nb
gdist7nb
gdist8nb
gdist9nb
gdist10nb
gdist11nb
gdist12nb
gdist13nb
gdist14nb
gdist15nb
gdist16nb
gdist17nb
gdist18nb

model.list<-list(gdist1nb,gdist2nb,gdist3nb,gdist4nb,gdist5nb,gdist6nb,gdist7nb,gdist8nb,
                 gdist9nb,gdist10nb,gdist11nb,gdist13nb,gdist14nb, gdist15nb,gdist16nb, gdist17nb)
modSel(fitList(fits=model.list))

## Top models are gdist5nb and gdist13nb
## gdist5nb has issues with identifying the PICO offset.  It looks like the SE is far too inflated to
## be accurate
## gdist13nb is only ~1 AIC point higher and has parameters that seem more reliable


plot(gdist13nb)
plot(gdist5nb)


###Bootstrap for model fit
summary(gdist13nb)

model.boot<-(pb<- parboot(gdist13nb,statistic=fitstats, nsim=20, report=5))


(c.hat<-pb@t0[2]/mean(pb@t.star[,2]))
model.boot

###Return estimates
n.pred<-predict(gdist5nb, type = "lambda")
groups.pred<- cbind(x$Site,x$Month,n.pred)

groups.pred

(.35^2)
0.1225*3.14159
0.3848448*5


groups.pred$new.preds<- groups.pred$Predicted/1.924224
groups.pred$SE.new<- groups.pred$SE/1.924224
groups.pred$lower.new<- groups.pred$lower/1.924224
groups.pred$upper.new<- groups.pred$upper/1.924224
groups.pred
#THis sums up the total number of birds observed across all visits
getN<- function(fm, newdata=NULL)
  sum(predict(fm,type="lambda",newdata=newdata)[,1])
getN(gdist13nb)

re.CLNU<-ranef(gdist13nb, k=150)
sum(bup(re.CLNU, "mean"))




###Include multiple survey periods

##separate out survey periods
clnu.8<-CLNUdist_revised2%>%
  filter(survey_period=="8")
clnu.9<-CLNUdist_revised2%>%
  filter(survey_period=="9")
clnu.10<-CLNUdist_revised2%>%
  filter(survey_period=="10")

##Create matrices for each survey period
dist.8<-formatDistData(clnu.8, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.8
dist.9<-formatDistData(clnu.9, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.9
dist.10<-formatDistData(clnu.10, distCol = "distance",
                        transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.10

dist.mat<-cbind(dist.8, dist.9, dist.10)

x.open <- data.frame("Site" = c("Avalanche Peak",
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

yearly.covs <- data.frame( "Month" = c("8","10","9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9", 
                                       "8", "10", "9" ),
                           "Observers" = c("TM","TM","TM", 
                                           "TM", "WW", "TM/DT", 
                                           "TM", "TM/DT", "TM", 
                                           "TM", "WW", "WW", 
                                           "TM", "TM/DT", "TM/DT", 
                                           "TM", "WW", "TM", 
                                           "TM", "WW", "WW",  
                                           "TM", "TM/DT", "DT",  
                                           "TM", "TM","TM/DT",  
                                           "TM", "WW","TM",  
                                           "TM", "WW", "TM"))

open.umf<- unmarkedFrameGDS(y= dist.mat,siteCovs = data.frame (x.open,area),
                            yearlySiteCovs = yearly.covs, numPrimary = 3,
                            dist.breaks=dist.bins.long, survey = "point",unitsIn = "km")
summary(open.umf)
str(open.umf)

open.dist1nb<-gdistsamp(lambdaformula = ~1,phiformula = ~1,pformula= ~1, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
open.dist2nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~1, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
open.dist3nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~Month,pformula= ~1, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
open.dist4nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~Observers, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
open.dist5nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~Month,pformula= ~Observers, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
summary(open.dist1nb)
summary(open.dist2nb)
summary(open.dist3nb)
summary(open.dist4nb)
summary(open.dist5nb)


model.list<-list(open.dist1nb, open.dist2nb, open.dist3nb, open.dist4nb, open.dist5nb)
modSel(fitList(fits=model.list))

### predictions
backTransform(open.dist5nb, type = "lambda")
predict(open.dist4nb, type = "lambda")
predict(gdist7nb, type = "lambda")

getN<- function(fm, newdata=NULL)
  sum(predict(fm,type="lambda",newdata=newdata)[,1])
getN(open.dist4nb)

re.CLNU<-ranef(open.dist4nb, k=150)
sum(bup(re.CLNU, "mean"))
