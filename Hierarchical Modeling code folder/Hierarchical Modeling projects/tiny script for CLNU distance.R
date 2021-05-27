dist.bins.long<- c(0.0,0.01,0.02,0.07,0.11,0.15,0.195,0.25,0.33,0.4,0.5)
dist.bins.alt<- c(0,0.05,0.2,0.3)

CDS1<-ds(CLNUdist_revised2, transect = "point", key = "unif", adjustment = "cos",
         truncation = 0.3, cutpoints = dist.bins.alt)
plot(CDS1, pdf = TRUE)
summary(CDS1)

CDS2<-ds(CLNUdist_revised2, transect = "point", key = "hn", adjustment = "herm",
         truncation = 0.3, cutpoints = dist.bins.alt)
plot(CDS2, pdf = TRUE)
summary(CDS2)

CDS3<-ds(CLNUdist_revised2, transect = "point", key = "hr", adjustment = "poly",
         truncation = 0.3, cutpoints = dist.bins.alt)
plot(CDS3, pdf = TRUE)
summary(CDS3)

CDStable<-summarize_ds_models(CDS1,CDS2,CDS3)

CDStable
