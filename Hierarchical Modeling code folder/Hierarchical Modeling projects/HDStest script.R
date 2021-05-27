library(unmarked)
library(tidyverse)

CLNUdist_revised2<-read.csv("CLNUdist_unmarked.csv")
str(CLNUdist_revised2)

CLNUdist_revised2$odistance<-CLNUdist_revised2$distance
CLNUdist_revised2$distance<-CLNUdist_revised2$distance/1000

dist.bins.long<-c(0.0,0.3,0.4,0.45)

area<- pi*250^2/1000000


test<-formatDistData(CLNUdist_revised2, distCol = "distance",
                     transectNameCol = "UMF_site", dist.breaks = dist.bins.long)
str(test)


x <- data.frame("Site" = c("Avalanche Peak","Avalanche Peak","Avalanche Peak", 
                                  "Baronette", "Baronette", "Baronette", "Baronette", 
                                  "Bear Jam", "Bear Jam", "Bear Jam", "Bear Jam",
                                  "Canyon", "Canyon", "Canyon", "Canyon",
                                  "Confluence", "Confluence", "Confluence", "Confluence",
                                  "DeLacy North", "DeLacy North", "DeLacy North", "DeLacy North",
                                  "DeLacy South", "DeLacy South", "DeLacy South", "DeLacy South",
                                  "Dunraven", "Dunraven", "Dunraven", "Dunraven",
                                  "Mammoth", "Mammoth", "Mammoth", "Mammoth",
                                  "Trout Lake", "Trout Lake", "Trout Lake", "Trout Lake",
                                  "West Thumb", "West Thumb", "West Thumb", "West Thumb"),
                "Forest_type" = c("PIAL","PIAL","PIAL", 
                                   "PIEN", "PIEN", "PIEN", "PIEN", 
                                   "PSME", "PSME", "PSME", "PSME",
                                   "PICO", "PICO", "PICO", "PICO",
                                   "PIFL", "PIFL", "PIFL", "PIFL",
                                   "PICO", "PICO", "PICO", "PICO",
                                   "PIAL", "PIAL", "PIAL", "PIAL",
                                   "PIAL", "PIAL", "PIAL", "PIAL",
                                   "PIFL", "PIFL", "PIFL", "PIFL",
                                   "PSME", "PSME", "PSME", "PSME",
                                   "PIEN", "PIEN", "PIEN", "PIEN"),
                "Month" = c("8","8","9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9", 
                            "8", "8", "10", "9" ),
                "Observers" = c("TM","TM","TM", 
                             "TM", "TM", "WW", "TM/DT", 
                             "TM", "TM", "TM/DT", "TM", 
                             "TM/DT", "TM", "WW", "WW", 
                             "TM", "TM", "TM/DT", "TM/DT", 
                             "TM", "TM", "WW", "TM", 
                             "TM", "TM", "WW", "WW",  
                             "TM", "TM", "TM/DT", "DT",  
                             "TM", "TM", "TM","TM/DT",  
                             "TM", "TM", "WW","TM",  
                             "TM", "TM", "WW", "TM"), 
                "Effort" = c("3","3","3", 
                            "4", "4", "4", "4", 
                            "4", "4", "4", "4", 
                            "4", "4", "4", "4", 
                            "4", "4", "4", "4", 
                            "4", "4", "4", "4", 
                            "4", "4", "4", "4",  
                            "4", "4", "4", "4",  
                            "4", "4", "4", "4",  
                            "4", "4", "4", "4",  
                            "4", "4", "4", "4" ))

str(x)

#x <- data.frame("Forest_type" = c("PIAL", "PIEN", "PSME", "PICO", "PIFL", "PICO", "PIAL",
#                                  "PIAL", "PIFL", "PSME", "PIEN"))

umftest<- unmarkedFrameDS(y= test,siteCovs = data.frame (x,area),
                          dist.breaks=dist.bins.long, survey = "point",unitsIn = "km")

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
                     transectNameCol = "UMF_site", dist.breaks = dist.bins.long)

gumftest<- unmarkedFrameGDS(y= test,siteCovs = data.frame (x,area), numPrimary = 1,
                          dist.breaks=dist.bins.long, survey = "point",unitsIn = "km")

summary(gumftest)
##Gdist models

gdist1nb<-gdistsamp(lambdaformula = ~1,phiformula = ~1,pformula= ~1, data = gumftest,
                  output = "density", unitsOut = "kmsq", mixture= "NB")

gdist2nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~1, data = gumftest,
                  output = "density", unitsOut = "kmsq", mixture= "NB")

gdist3nb<-gdistsamp(lambdaformula = ~Month,phiformula = ~1,pformula= ~1, data = gumftest,
                  output = "density", unitsOut = "kmsq", mixture= "NB")

gdist4nb<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,pformula= ~1, data = gumftest,
                  output = "density", unitsOut = "kmsq", mixture= "NB")

gdist5nb<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,pformula= ~Forest_type, data = gumftest,
                  output = "density", unitsOut = "kmsq", mixture= "NB")

gdist6nb<-gdistsamp(lambdaformula = ~Forest_type + Month -1,phiformula = ~1,pformula= ~Month, data = gumftest,
                  output = "density", unitsOut = "kmsq", mixture= "NB")

gdist7nb<-gdistsamp(lambdaformula = ~Forest_type * Month,phiformula = ~1,pformula= ~Month, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")

gdist8nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~Month, data = gumftest,
                    output = "density", unitsOut = "kmsq", mixture= "NB")


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

gdist1pois
gdist2pois
gdist3pois
gdist4pois
gdist5pois


model.list<-list(gdist1nb,gdist2nb,gdist3nb,gdist4nb,gdist5nb,gdist6nb,gdist7nb,gdist1pois,gdist2pois,
        gdist3pois,gdist4pois,gdist5pois)
modSel(fitList(fits=model.list))

###Bootstrap for model fit
summary(gdist6nb)

model.boot<-(pb<- parboot(gdist6nb,statistic=fitstats, nsim=1000, report=5))
(c.hat<-pb@t0[2]/mean(pb@t.star[,2]))
model.boot

###Return estimates
n.pred<-predict(gdist6nb, type = "lambda")
groups.pred<- cbind(x$Site,x$Month,n.pred)

groups.pred

(.45^2)
0.2025*3.14159
0.636*5


groups.pred$new.preds<- groups.pred$Predicted/3.18
#THis sums up the total number of birds observed across all visits
getN<- function(fm, newdata=NULL)
   sum(predict(fm,type="lambda",newdata=newdata)[,1])
getN(gdist6nb)

re.CLNU<-ranef(gdist6nb, k=150)
sum(bup(re.CLNU, "mean"))




###Include multiple survey periods

##separate out survey periods
clnu.8<-CLNUdist_revised2%>%
  filter(Month=="July")
clnu.8.2<-CLNUdist_revised2%>%
  filter(Month=="August")
clnu.9<-CLNUdist_revised2%>%
  filter(Month=="September")
clnu.10<-CLNUdist_revised2%>%
  filter(Month=="October")

##Create matrices for each survey period
dist.8<-formatDistData(clnu.8, distCol = "distance",
                      transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.8
dist.8.2<-formatDistData(clnu.8.2, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.8.2
dist.9<-formatDistData(clnu.9, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.9
dist.10<-formatDistData(clnu.10, distCol = "distance",
                       transectNameCol = "Sample.Label", dist.breaks = dist.bins.long)
dist.10

dist.mat<-cbind(dist.8,dist.8.2, dist.9, dist.10)

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

yearly.covs <- data.frame( "Month" = c("8","8","10","9", 
                                       "8","8", "10", "9", 
                                       "8", "8", "10", "9", 
                                       "8","8", "10", "9", 
                                       "8","8", "10", "9", 
                                       "8", "8", "10", "9", 
                                       "8","8", "10", "9", 
                                       "8","8", "10", "9", 
                                       "8","8", "10", "9", 
                                       "8","8", "10", "9", 
                                       "8","8", "10", "9" ))
 ###               "Observers" = c("TM","TM","TM", 
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
                            yearlySiteCovs = yearly.covs, numPrimary = 4,
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
open.dist6nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~Forest_type, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")
open.dist6nb<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~Forest_type +Month, data = open.umf,
                        output = "density", unitsOut = "kmsq", mixture= "NB")


summary(open.dist1nb)
summary(open.dist2nb)
summary(open.dist3nb)
summary(open.dist4nb)
summary(open.dist5nb)
summary(open.dist6nb)


model.list<-list(open.dist1nb, open.dist2nb, open.dist3nb, open.dist4nb, open.dist5nb)
modSel(fitList(fits=model.list))

### predictions
backTransform(open.dist6nb, type = "lambda")
predict(open.dist6nb, type = "lambda")
predict(gdist7nb, type = "lambda")

getN<- function(fm, newdata=NULL)
  sum(predict(fm,type="lambda",newdata=newdata)[,1])
getN(open.dist4nb)

re.CLNU<-ranef(open.dist5nb, k=150)
sum(bup(re.CLNU, "mean"))
