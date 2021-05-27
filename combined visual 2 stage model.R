library(MASS)
library(Distance)
library(tidyverse)
library(AICcmodavg)
library(sjPlot)
library(knitr)
library(kableExtra)
library(magrittr)
library(webshot)
library(lubridate)

CLNU.dat<-read.csv("CLNU_data_summary.csv")
CLNU.dat<-CLNU_data_summary
distance.dat <-read.csv ("dist.tot.csv")
distance.dat<-dist.tot
distance.dat<- dist_5mins
distance.data.frame<-read.csv("CLNUdist_unmarked_repeats_removed.csv")

str(CLNU.dat)
CLNU.dat$date<-mdy(CLNU.dat$Date)
x<-CLNU.dat
###create an independent identifier for each site
x$survey <- with(CLNU.dat, interaction(Site, date))
x <- x[order(x$Site,x$Month),]
#distance.dat <- distance.dat[order(distance.dat$UMF_site),]
#survey<-unique(distance.data.frame$UMF_site, incomparables = FALSE)

#survey<-as.data.frame(survey)

#prep distance data
#distance.dat$odistance<-distance.dat$distance
#distance.dat$distance<-distance.dat$distance/1000
str(distance.dat)

distance.dat$survey <- with(distance.dat, interaction(Sample.Label, date))
distance.dat <- distance.dat[order(distance.dat$Sample.Label,distance.dat$Month),]

#distance.dat<-distance.dat%>%
  filter(distance<=0.07)

str(distance.dat)
hist(distance.dat$distance)

###aggregate individual obs into counts
dist.count<-distance.dat %>% count(survey)
dist.count
sum(dist.count$n)

#x<-cbind(x, survey)
#x <-subset(x,select= -c(survey))

x

CLNU.data <- merge(x, dist.count, by.x = "survey", 
                   by.y = "survey", all.x = TRUE, all.y = FALSE)
str(CLNU.data)
### convert NAs to zeros

CLNU.data[is.na(CLNU.data)] <- 0
sum(CLNU.data$n)
### Compare discrepancy in values of n and birds
x<-as.data.frame(x)
temp.x<-CLNU.data[, c(1,4,11)]
### Upon inspection there were 3 values that did not get transferred to the count df
### dunraven 10/10/20 , Delacy s 8/19/19, and Delacy s 9/7/19
### hard code these vals in
new.clnu<-CLNU.data
CLNU.data[62, 11] = 16
CLNU.data[4, 11] = 5
CLNU.data[12, 11] = 2
sum(CLNU.data$n)
###Compare values to as a gut check to ensure our data management code worked
CLNU.data$diffs<-CLNU.data$Birds - CLNU.data$n

hist(CLNU.data$n, breaks = 50)
sum(CLNU.data$n)
###calculate detectability with distance and collect into a list

dist.bins4<- c(0.0,0.01,0.04,0.06,0.07)

dist.tot$Area<- 77.715

CDS10<-ds(dist.tot, transect = "point", key = "unif", adjustment = "cos",
          truncation = .07, cutpoints = dist.bins4)
plot(CDS10, pdf = TRUE)
summary(CDS10)

CDS11<-ds(dist.tot, transect = "point", key = "hn", adjustment = "herm",
          truncation = .07, cutpoints = dist.bins4)
plot(CDS11, pdf = TRUE)
summary(CDS11)

CDS12<-ds(dist.tot, transect = "point", key = "hr", adjustment = "poly",
          truncation = .07, cutpoints = dist.bins4)
plot(CDS12, pdf = TRUE)
summary(CDS12)
###MCDS models
###variables: forest type, month
str(dist.tot)
dist.tot$Month<-as.factor(dist.tot$Month)

MCDS1<-ds(dist.tot, ~ Forest_type, transect = "point", key = "hn", adjustment = "herm",
          truncation = .07, cutpoints = dist.bins4)
plot(MCDS1, pdf = TRUE)
summary(MCDS1)

MCDS2<-ds(dist.tot, ~ Forest_type, transect = "point", key = "hr", adjustment = "poly",
          truncation = .07, cutpoints = dist.bins4)
plot(MCDS2, pdf = TRUE)
summary(MCDS2)

MCDS3<-ds(dist.tot, ~ Month, transect = "point", key = "hn", adjustment = "herm",
          truncation = .07, cutpoints = dist.bins4)
plot(MCDS3, pdf = TRUE)
summary(MCDS3)

MCDS4<-ds(dist.tot, ~ Month, transect = "point", key = "hr", adjustment = "poly",
          truncation = .07, cutpoints = dist.bins4)
plot(MCDS4, pdf = TRUE)
summary(MCDS4)

MCDS5<-ds(dist.tot, ~ Month + Forest_type, transect = "point", key = "hn", adjustment = "herm",
          truncation = .07, cutpoints = dist.bins4)
plot(MCDS5, pdf = TRUE)
summary(MCDS5)

MCDS6<-ds(dist.tot, ~ Month + Forest_type, transect = "point", key = "hr", adjustment = "poly",
          truncation = .07, cutpoints = dist.bins4)
plot(MCDS6, pdf = TRUE)
summary(MCDS6)


CDStable4<-summarize_ds_models(CDS10, CDS11, CDS12, MCDS1, MCDS2, MCDS3, MCDS4, MCDS5, MCDS6)

CDStable4


#### scoop up all detectabilities
MCDS1[["ddf"]][["fitted"]]->MCDS.det.long
MCDS.det.long<-as.data.frame(MCDS.det.long)

MCDS.det<-unique(MCDS.det.long$MCDS.det.long, incomparables = FALSE)
MCDS.det

Forest_type<-c("PICO","PIAL","PSME", "PIFL","PIEN")

det.frame<-cbind(MCDS.det,Forest_type)
det.frame<- as.data.frame(det.frame)

CLNU.data2 <- merge(CLNU.data, det.frame, by.x = "Forest_Type", 
                    by.y = "Forest_type", all.x = FALSE, all.y = FALSE)
CLNU.data2$MCDS.det<-as.numeric(levels(CLNU.data2$MCDS.det))[CLNU.data2$MCDS.det]
CLNU.data<-CLNU.data2

CLNU.data$Month<- as.factor(CLNU.data$Month)
### run models
circle<-3.14159*.07^2
density_result<-circle*5
density_result

### during each survey we cover 0.076km2



zero<-glm.nb(n ~ 1 + offset (log(MCDS.det)), data = CLNU.data)

one<-glm.nb(n ~ Forest_Type + (log(MCDS.det)), data = CLNU.data)

two<-glm.nb(n ~ Month + offset (log(MCDS.det)), data = CLNU.data)

three<- glm.nb (n ~ Forest_Type + Month + offset (log(MCDS.det)), data = CLNU.data)

four<- glm.nb (n ~ Forest_Type * Month + offset (log(MCDS.det)), data = CLNU.data)

five<- glm.nb (n ~ Forest_Type + Total_Cones + offset (log(MCDS.det)), data = CLNU.data)

six <- glm.nb (n ~ Forest_Type * Total_Cones + offset (log(MCDS.det)), data = CLNU.data)

seven<- glm.nb (n ~ Forest_Type * Month * Total_Cones + offset (log(MCDS.det)), data = CLNU.data)

eight<- glm.nb (n ~ Forest_Type * Month + Total_Cones + offset (log(MCDS.det)), data = CLNU.data)

nine<- glm.nb (n ~ Forest_Type + Month + Total_Cones + offset (log(MCDS.det)), data = CLNU.data)



###model list

glm.list <- list(zero,one,two,three,four,five,six,seven,eight,nine)
glm.names <- as.character(unlist(lapply(glm.list,formula)))
(glm.results <- aictab(glm.list,modnames=glm.names))

summary(three)
summary(nine)


(est <- cbind(Estimate = coef(three), confint(three)))
exp(est)


plot_model(three, type = "pred",terms = c("Forest_Type","Month"))
