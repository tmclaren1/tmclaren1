library(Distance)

dist_5mins$Month<-as.factor(dist_5mins$Month)

hist(dist_5mins$distance)

CDS1<-ds(dist_10mins, transect = "point", key = "unif", adjustment = "cos",
         truncation = .2)
plot(CDS1, pdf = TRUE)
summary(CDS1)

CDS2<-ds(dist_10mins, transect = "point", key = "hn", adjustment = "herm",
         truncation = .2)
plot(CDS2, pdf = TRUE)
summary(CDS2)

MCDStable<-summarize_ds_models(CDS1,CDS2)

MCDStable


five.min.bins<- c(0.0,0.17,0.25,0.4)

hist(dist_5mins$distance)
str(dist_5mins)

CDS3<-ds(dist_5mins, transect = "point", key = "unif", adjustment = "cos",
         truncation = .4, cutpoints = five.min.bins)
plot(CDS3, pdf = TRUE)
summary(CDS3)

CDS4<-ds(dist_5mins, transect = "point", key = "hn", adjustment = "herm",
         truncation = .4, cutpoints = five.min.bins)
plot(CDS4, pdf = TRUE)
summary(CDS4)

MCDStable<-summarize_ds_models(CDS3,CDS4)

MCDStable

