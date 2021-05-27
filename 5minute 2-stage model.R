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
x
#distance.dat <- distance.dat[order(distance.dat$UMF_site),]
#survey<-unique(distance.data.frame$UMF_site, incomparables = FALSE)

#survey<-as.data.frame(survey)

#prep distance data
#distance.dat$odistance<-distance.dat$distance
#distance.dat$distance<-distance.dat$distance/1000
str(distance.dat)

distance.dat$date<-mdy(distance.dat$date)
distance.dat$survey <- with(distance.dat, interaction(Sample.Label, date))
distance.dat <- distance.dat[order(distance.dat$Sample.Label,distance.dat$Month),]

#distance.dat<-distance.dat%>%
#subset(distance<=0.35)
distance.dat<-distance.dat %>%
  filter((distance<=0.35)) 
           

str(distance.dat)
hist(distance.dat$distance)

###aggregate individual obs into counts
dist.count<-distance.dat %>% count(survey)
dist.count
sum(dist.count$n)
hist(dist.count$n, breaks = 15)
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
CLNU.data[62, 11] = 1
CLNU.data[4, 11] = 5
CLNU.data[12, 11] = 2
sum(CLNU.data$n)
###Compare values to as a gut check to ensure our data management code worked
CLNU.data$diffs<-CLNU.data$Birds - CLNU.data$n

hist(CLNU.data$n, breaks = 50)
sum(CLNU.data$n)
###calculate detectability with distance and collect into a list
five.min.bins<- c(0.0,0.17,0.25,0.35)

hist(distance.dat$distance)
str(distance.dat)

CDS3<-ds(distance.dat, transect = "point", key = "unif", adjustment = "cos",
         truncation = .35, cutpoints = five.min.bins)
plot(CDS3, pdf = TRUE)
summary(CDS3)

CDS5<-ds(distance.dat, transect = "point", key = "hn", adjustment = "herm",
         truncation = .35, cutpoints = five.min.bins)
plot(CDS5, pdf = TRUE)
summary(CDS5)
str(CDS5)

MCDS1<-ds(distance.dat,formula = ~Forest_type, transect = "point", key = "unif", adjustment = "cos",
         truncation = .35, cutpoints = five.min.bins)
plot(MCDS1, pdf = TRUE)
summary(MCDS1)

MCDS2<-ds(distance.dat, formula = ~Forest_type, transect = "point", key = "hn", adjustment = "herm",
         truncation = .35, cutpoints = five.min.bins)
plot(MCDS1, pdf = TRUE)
summary(MCDS1)


MCDS3<-ds(distance.dat, formula = ~Month, transect = "point", key = "hn", adjustment = "herm",
          truncation = .35, cutpoints = five.min.bins)
plot(MCDS3, pdf = TRUE)
summary(MCDS3)

MCDS4<-ds(distance.dat, formula = ~Observers, transect = "point", key = "hn", adjustment = "herm",
          truncation = .35, cutpoints = five.min.bins)
plot(MCDS4, pdf = TRUE)
summary(MCDS4)

MCDS5<-ds(distance.dat, formula = ~Year, transect = "point", key = "hn", adjustment = "herm",
          truncation = .35, cutpoints = five.min.bins)
plot(MCDS5, pdf = TRUE)
summary(MCDS5)


MCDStable<-summarize_ds_models(CDS3,CDS5,MCDS3,MCDS4)

MCDStable

####Scoop up detectabilities
MCDS3[["ddf"]][["fitted"]]->MCDS.det.long
MCDS.det.long<-as.data.frame(MCDS.det.long)

MCDS.det<-unique(MCDS.det.long$MCDS.det.long, incomparables = FALSE)
MCDS.det

Month<-c("9","10","8")

det.frame<-cbind(MCDS.det,Month)
det.frame<- as.data.frame(det.frame)
det.frame

####Attach to Count Data Frame
CLNU.data2 <- merge(CLNU.data, det.frame, by.x = "Month", 
                    by.y = "Month", all.x = FALSE, all.y = FALSE)
CLNU.data2$MCDS.det<-as.numeric(levels(CLNU.data2$MCDS.det))[CLNU.data2$MCDS.det]
CLNU.data<-CLNU.data2
CLNU.data
#####Running MCDS with Month variable is lowest AIC
#####Try using Month detectabilities as offsets



detection<-0.1118028


#### scoop up all detectabilities
CLNU.data$detection<-detection
CLNU.data$MCDS.det<-detection

### run models
circle<-3.14159*.35^2
density_result<-circle*5
density_result

### during each survey we cover 0.076km2

CLNU.data$Month<-as.factor(CLNU.data$month)
#adjust for surveyed area
CLNU.data$n<-CLNU.data$n/density_result
hist(CLNU.data$n, breaks = 40, main = "Nutcracker Counts per survey", xlab = "n")


zero<-glm.nb(n ~ 1 + offset (log(MCDS.det)), data = CLNU.data)

one<-glm.nb(n ~ Forest_Type + offset (log(MCDS.det)), data = CLNU.data)

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
#glm.names <- as.character(unlist(lapply(glm.list,formula)))
glm.names<-c("Null", "Forest Type only", "Month only", "Forest Type + Month", "Forest Type * Month",
             "Forest Type + Total Cones", "Forest Type * Total Cones", "Forest Type * Month * Total Cones",
             "Forest Type * Month + Total Cones", "Forest Type + Month + Total Cones")
(glm.results <- aictab(glm.list,modnames=glm.names))

kable(glm.results)

summary(three)
summary(five)
summary(nine)
summary(one)
summary(six)

plot(five)
plot(three)
plot(nine)
plot(one)
plot(six)


(est <- cbind(Estimate = coef(three), confint(three)))
exp(est)


plot_model(five, type = "pred",terms = c("Forest_Type","Total_Cones"))
plot_model(five, type = "pred",terms = c("Total_Cones","Forest_Type"))
plot_model(nine, type = "pred",terms = c("Forest_Type","Month"))
plot_model(three, type = "pred", terms = c("Forest_Type", "Month"))
plot_model(one, type = "pred", terms = c("Forest_Type"))
