library(MASS)
library(Distance)
library(tidyverse)
library(AICcmodavg)
library(sjPlot)
library(knitr)
library(kableExtra)
library(magrittr)
library(webshot)

CLNU.dat<-read.csv("CLNU_data_summary.csv")
distance.dat <-read.csv ("CLNUdist_unmarked_removed_repeats.csv")

CLNU.dat<-CLNU.dat%>%
  filter(Year=="2020")

str(CLNU.dat)
x <- CLNU.dat[order(CLNU.dat$Site,CLNU.dat$month),]
###create an independent identifier for each site
x$survey <- with(CLNU.dat, interaction(Site,  month))
distance.dat <- distance.dat[order(distance.dat$UMF_site),]
survey<-unique(distance.dat$UMF_site, incomparables = FALSE)

survey<-as.data.frame(survey)

#prep distance data
distance.dat$odistance<-distance.dat$distance
distance.dat$distance<-distance.dat$distance/1000
str(distance.dat)

distance.dat$survey <- with(distance.dat, interaction(Sample.Label,  survey_period))

distance.dat<-distance.dat%>%
  filter(distance<=0.45)

str(distance.dat)

###aggregate individual obs into counts

dist.count<-distance.dat %>% count(UMF_site)
dist.count


x<-cbind(x, survey)
x <-subset(x,select= -c(survey))

x

CLNU.data <- merge(x, dist.count, by.x = "survey", 
                   by.y = "UMF_site", all.x = TRUE, all.y = FALSE)
str(CLNU.data)
### convert NAs to zeros

CLNU.data[is.na(CLNU.data)] <- 0

###Compare values to as a gut check to ensure our data management code worked
CLNU.data$diffs<-CLNU.data$Birds - CLNU.data$n

###calculate detectability with distance and collect into it
dist.bins5<-c(0.0,0.3,0.4,0.45)

CDS1<-ds(distance.dat, transect = "point", key = "unif", adjustment = "cos",
         truncation = .45, cutpoints = dist.bins5)
plot(CDS1, pdf = TRUE)
summary(CDS1)

CDS2<-ds(distance.dat, transect = "point", key = "hn", adjustment = "herm",
         truncation = .45, cutpoints = dist.bins5)
plot(CDS2, pdf = TRUE)
summary(CDS2)

CDS3<-ds(distance.dat, transect = "point", key = "hr", adjustment = "poly",
         truncation = .45, cutpoints = dist.bins5)
plot(CDS3, pdf = TRUE)
summary(CDS3)

CDStable2<-summarize_ds_models(CDS1,CDS2,CDS3)

CDStable2
###Put MCDS models here
distance.dat$survey_period<-as.factor(distance.dat$survey_period)

MCDS2<-ds(distance.dat, formula = ~Forest_type, transect = "point", key = "hn",
          truncation = .45, cutpoints = dist.bins5)
plot(MCDS2, pdf = TRUE)
summary(MCDS2)

MCDS3<-ds(distance.dat, formula = ~Forest_type, transect = "point", key = "hr",
          truncation = .45, cutpoints = dist.bins5)
plot(MCDS3, pdf = TRUE)
summary(MCDS3)

MCDS4<-ds(distance.dat, formula = ~survey_period, transect = "point", key = "hn",
          truncation = .45, cutpoints = dist.bins5)
summary(MCDS4)
plot(MCDS4,pdf = TRUE)

MCDS5<-ds(distance.dat, formula = ~survey_period, transect = "point", key = "hr",
          truncation = .45, cutpoints = dist.bins5)
summary(MCDS5)

MCDStable<-summarize_ds_models(CDS1,CDS2,CDS3,MCDS2,MCDS3,MCDS4,MCDS5)

MCDStable

#best model is cds2 or MCDS4(survey period = month)
MCDS4[["ddf"]][["fitted"]]->MCDS.det.long
MCDS.det.long<-as.data.frame(MCDS.det.long)
###Compare values in long form to check which values correspond to which months
survey_period<-as.data.frame(distance.dat$survey_period)
det.to.surv<-cbind(MCDS.det.long,survey_period)
det.to.surv$survey_period<-det.to.surv$`distance.dat$survey_period`


###Create a shorter data frame to impute values into CLNU.data data frame
MCDS.det<-unique(MCDS.det.long, incomparables = FALSE)
MCDS.det

#MCDS.det<-c(0.365923028738347,0.173799430745523,0.147491616032556)
MCDS.det<-c(0.365,0.173,0.147)
Months<-c("8","9","10")
MCDS.det<-cbind(Months,MCDS.det)
MCDS.det<-as.data.frame(MCDS.det)


### Create table of detectability
colnames(MCDS.det) <- c("Month", "$\\text{Detectability}(\\hat{P}$)")
signif(MCDS.det$`$\text{Detectability}(\hat{P}$)`,3)
kable(MCDS.det)


#Add det.to.survey to CLNU.data and convert back to numeric values
CLNU.data2 <- merge(CLNU.data, MCDS.det, by.x = "Month", 
                   by.y = "Months", all.x = FALSE, all.y = FALSE)
CLNU.data2$MCDS.det<-as.numeric(levels(CLNU.data2$MCDS.det))[CLNU.data2$MCDS.det]

CLNU.data<-CLNU.data2

CLNU.data$Month<-as.factor(CLNU.data$Month)



MCDS.det<-unique(MCDS.det.long, incomparables = FALSE)
MCDS.det

#### Create tables of MCDS output
clnu_table <- summary(MCDS4)$dht$individuals$D
clnu_table$lcl <- clnu_table$ucl <- clnu_table$df <- NULL
colnames(clnu_table) <- c("Stratum", "$\\hat{D}$", "$\\text{se}(\\hat{D}$)",
                          "$\\text{CV}(\\hat{D}$)")
kable(clnu_table)


hist(CLNU.data$n, main = "Clark's nutcracker counts", xlab = "Number of observations")
# best CDS model detectability as an alternative
#CDS2[["ddf"]][["fitted"]][["1"]]->detection

#detect<-rep(detection,43)

#CLNU.data<-cbind(CLNU.data,detect)


###Analyze data with NB regression
###I'm not entirely sure if I coded the offset correctly here.
### I also still need to think about exactly what the resulting estimates relate to
### I think this is density across 5 separate 450m radius circles but I need to
### think about it a little more to be sure.
circle<-3.14159*450^2
density_result<-circle*5
density_result
KMs<-density_result/1000^2
KMs
# this results in an area of 3.18 square kilometers covered per transect
# If we divide each estimate by 3.18 square km that should be our density per km


zero<-glm.nb(n ~ 1 + offset (log(MCDS.det)), data = CLNU.data)

one<-glm.nb(n ~ Forest_Type-1 + (log(MCDS.det)), data = CLNU.data)

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

#Model 5 appears to be the most parsimonious but there are quite a few with competetive
#AIC scores
summary(one)
summary(three)
summary(five)

plot_model(one, type = "pred",terms = c("Forest_Type"))

plot_model(three, type = "pred",terms = c("Forest_Type","Month"))

###predict values
fitted(one)
predict(one)

predict(one, type = "response")
(est <- cbind(Estimate = coef(one), confint(one)))
exp(est)
glm.estimates<-as.data.frame(exp(est)/KMs)

conifer_species<-as.vector(conifer_species)
glm.table<-cbind(glm.estimates,conifer_species)
glm.table<-glm.table[-c( 6), ] 
glm.table<-glm.table[-c,(1) ] 
names(glm.table)[2] <- "lower CI"
names(glm.table)[3] <- "upper CI"
glm.table$lower<-glm.table$Estimate-glm.table$lcl
glm.table$upper<-glm.table$ucl-glm.table$Estimate

glm.plot<- ggplot(glm.table, aes(x=conifer_species, y=Estimate)) +
  geom_point(size = 3)+
  geom_pointrange(aes(ymin=Estimate-lower, ymax=Estimate+upper))+
  labs(x = "Forest type", y= "Predictions")+
theme_light()
glm.plot  

### Preds table
glm.table
glm.table<-cbind(conifer_species,glm.table$Estimate,
                 glm.table$`lower CI`,glm.table$`upper CI`)
colnames(glm.table) <- c("Forest Type","Prediction", "Lower 95% CI", "Upper 95% CI")


Species<-c("Whitebark pine", "Lodgepole pine", "Engelmann spruce","Limber pine", "Douglas-fir")
FT<-as.factor(Forest_type)
Estimate<-c(3.39,1.31,0.89,1.77,1.26)                       
lower<- c(1.22,0.46,0.27,0.53,0.42)
upper<- c(9.98,3.93,3.04,6.21,4.02)

pred.tab<-cbind(Species,Estimate,lower,upper)
