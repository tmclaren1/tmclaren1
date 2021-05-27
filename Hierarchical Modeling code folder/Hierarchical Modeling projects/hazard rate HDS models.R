HDSnb.haz1<-gdistsamp(lambdaformula = ~1,phiformula = ~1,pformula= ~1,
                    data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz2<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,
                    pformula= ~1, data = gumftest,  keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz3<-gdistsamp(lambdaformula = ~Month,phiformula = ~1,pformula= ~1, 
                    data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz4<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,
                    pformula= ~1, data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz5<-gdistsamp(lambdaformula = ~Forest_type + Month,phiformula = ~1,
                    pformula= ~Forest_type, data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz6<-gdistsamp(lambdaformula = ~Forest_type + Month ,phiformula = ~1,
                    pformula= ~Month, data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz7<-gdistsamp(lambdaformula = ~Forest_type * Month,phiformula = ~1,
                    pformula= ~Month, data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz8<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,
                    pformula= ~Month, data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz9<-gdistsamp(lambdaformula = ~Forest_type +Tot_Cones1,phiformula = ~1,
                    pformula= ~Month, data = gumftest, keyfun = "hazard",
                    output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz10<-gdistsamp(lambdaformula = ~Forest_type *Tot_Cones1,phiformula = ~1,
                     pformula= ~1, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz11<-gdistsamp(lambdaformula = ~Forest_type *Tot_Cones1,phiformula = ~1,
                     pformula= ~Month, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz12<-gdistsamp(lambdaformula = ~Forest_type *Tot_Cones1,phiformula = ~1,
                     pformula= ~Forest_type, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB",
                     starts = c())

HDSnb.haz13<-gdistsamp(lambdaformula = ~Forest_type + Tot_Cones,phiformula = ~1,
                     pformula= ~Forest_type, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz14<-gdistsamp(lambdaformula = ~Forest_type + Tot_Cones1,phiformula = ~1 ,
                     pformula= ~1, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz15<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1 ,
                     pformula= ~Observers, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz16<-gdistsamp(lambdaformula = ~Forest_type +Month +Tot_Cones1,phiformula = ~1 ,
                     pformula= ~1, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")


HDSnb.haz17<-gdistsamp(lambdaformula = ~Forest_type +Tot_Cones +Month,phiformula = ~1 ,
                     pformula= ~Forest_type, data = gumftest, keyfun = "hazard",
                     output = "density", unitsOut = "kmsq", mixture= "NB")

HDSnb.haz18<-gdistsamp(lambdaformula = ~Tot_Cones1,phiformula = ~1,pformula= ~Forest_type,
                      data = gumftest, keyfun = "hazard",
                      output = "density", unitsOut = "kmsq", mixture= "NB")
HDSnb.haz19<-gdistsamp(lambdaformula = ~Tot_Cones0,phiformula = ~1,pformula= ~Forest_type,
                       data = gumftest, keyfun = "hazard",
                       output = "density", unitsOut = "kmsq", mixture= "NB")
HDSnb.haz20<-gdistsamp(lambdaformula = ~Tot_Cones2,phiformula = ~1,pformula= ~Forest_type,
                       data = gumftest, keyfun = "hazard",
                       output = "density", unitsOut = "kmsq", mixture= "NB")
HDSnb.haz21<-gdistsamp(lambdaformula = ~Tot_Cones,phiformula = ~1,pformula= ~Forest_type,
                       data = gumftest, keyfun = "hazard",
                       output = "density", unitsOut = "kmsq", mixture= "NB")
HDSnb.haz22<-gdistsamp(lambdaformula = ~Forest_type,phiformula = ~1,pformula= ~Forest_type,
                       data = gumftest, keyfun = "hazard",
                       output = "density", unitsOut = "kmsq", mixture= "NB",
                       starts = c(1,-1,-1,-1,-1,0,-2,-1,0,0,0,0))

plot(HDSnb.haz18)


model.list<-list(HDSnb.haz1,HDSnb.haz2,HDSnb.haz3,HDSnb.haz4,HDSnb.haz5,HDSnb.haz6,
                 HDSnb.haz7,HDSnb.haz8,HDSnb.haz9,HDSnb.haz10,HDSnb.haz11,
                 #HDSnb.haz12,
                 HDSnb.haz13,HDSnb.haz14,HDSnb.haz15,HDSnb.haz16,HDSnb.haz17,
                 HDSnb.haz18,HDSnb.haz19)
modSel(fitList(fits=model.list))

HDSnb.haz1
HDSnb.haz5
HDSnb.haz13
HDSnb.haz17
HDSnb.haz18
HDSnb.haz19
HDSnb.haz20
HDSnb.haz21
HDSnb.haz22

exp(3.133592)
exp(3.133592 + .3)

plot(HDSnb.haz21)
plot(HDSnb.haz5)
plot(HDSnb.haz18)
plot(HDSnb.haz22)


summary(HDSnb.haz22)

model.boot<-(pb<- parboot(HDSnb.haz19,statistic=fitstats, nsim=20, report=5))


(c.hat<-pb@t0[2]/mean(pb@t.star[,2]))
model.boot

n.pred<-predict(HDSnb.haz21, type = "lambda")
groups.pred<- cbind(x$Site,x$Month,n.pred)

groups.pred


###combined model list


model.list<-list(HDSnb.haz1,HDSnb.haz2,HDSnb.haz3,HDSnb.haz4,HDSnb.haz5,HDSnb.haz6,
                 HDSnb.haz7,HDSnb.haz8,HDSnb.haz9,HDSnb.haz10,HDSnb.haz11,
                 #HDSnb.haz12,
                 HDSnb.haz13,HDSnb.haz14,HDSnb.haz15,HDSnb.haz16,HDSnb.haz17, HDSnb.haz18,
                 gdist1nb,gdist2nb,gdist3nb,gdist4nb,gdist5nb,gdist6nb,gdist7nb,gdist8nb,
                 gdist9nb,gdist10nb,gdist11nb,gdist13nb,gdist14nb, gdist15nb,gdist16nb, gdist17nb)
modSel(fitList(fits=model.list))

str(mods)

(toExport <- as(mods, "data.frame"))

toExport1<-as.data.frame(cbind(toExport$formula,toExport$AIC, toExport$delta,
                           toExport$AICwt,toExport$cumltvWt))

toExport1
