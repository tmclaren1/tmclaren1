###MCDS for 2 stage model
library(Distance)
###reformat effort
distance.dat$o_effort<-distance.dat$Effort
distance.dat$Effort<-1
distance.dat$Effort[which(distance.dat$survey_period == "8")] = "2"
distance.dat$Effort<-as.integer(distance.dat$Effort)

distance.dat$o_sample.label<-distance.dat$Sample.Label
distance.dat$Sample.Label<-distance.dat$survey

str(distance.dat)

hist(CLNUdist_revised2$distance, xlab = "Distance (km)", main = "Histogram of detections, 2020")
plot(CDS3, pdf = TRUE)


distance.dat[,'survey_period']<-factor(distance.dat[,'survey_period'])
str(distance.dat)
dist.bins5<-c(0.0,0.3,0.4,0.45)


MCDS2<-ds(distance.dat, formula = ~Forest_type, transect = "point", key = "hn",
         truncation = .45, cutpoints = dist.bins5)
plot(MCDS2, pdf = TRUE)
summary(MCDS2)

MCDS3<-ds(distance.dat, formula = ~Forest_type, transect = "point", key = "hr",
         truncation = .45, cutpoints = dist.bins5)
plot(MCDS3, pdf = TRUE)
summary(MCDS3)

O_MCDS4<-ds(distance.dat, formula = ~survey_period, transect = "point", key = "hn",
          truncation = .45, cutpoints = dist.bins5)
summary(O_MCDS4)
plot(MCDS4,pdf = TRUE)

MCDS5<-ds(distance.dat, formula = ~survey_period, transect = "point", key = "hr",
          truncation = .45, cutpoints = dist.bins5)
summary(MCDS5)

MCDS6<-ds(distance.dat, formula = ~Observers, transect = "point", key = "hn",
          truncation = .45, cutpoints = dist.bins5)
summary(MCDS6)
plot(MCDS6,pdf = TRUE)

MCDS7<-ds(distance.dat, formula = ~Observers, transect = "point", key = "hr",
          truncation = .45, cutpoints = dist.bins5)
summary(MCDS7)
plot(MCDS7, pdf = TRUE)

MCDStable<-summarize_ds_models(CDS1,CDS2,CDS3,MCDS2,MCDS3,MCDS4,MCDS5)

MCDStable
