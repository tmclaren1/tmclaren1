###Simulation Code from Applied Hierarchical Modeling###
### Adapted from Royle and Kery 2015###
### Written by Tommy McLaren###

M<- 267 #Spatial replicates
J<- 3  #Temporal replicates

elev<- runif(n=M, -1,1) #vector for elevation
forest<- runif(n=M, -1,1) #vector for forest cover
wind<- array(runif(n=M*J, -1,1), dim = c(M,J)) #array for wind speed


###Abundance for species

mean.lambda<- 2 #mean expected abundance for spp
beta0<- log(mean.lambda) #Same on log scale
beta1<--2 #Effect (slope) of elevation
beta2<-2 #Effect(slope) of forest cover
beta3<-1 #Interaction effect of elev and forest



###Visualization of the relationships between variables and abundance
log.lambda<- beta0 + beta1*elev + beta2*forest + beta3*elev*forest
lambda<- exp(log.lambda)

par(mfrow=c(2,2), mar=c(5,4,2,2), cex.main=1)
curve(exp(beta0+beta1*x), -1,1,col="red", frame.plot=FALSE,ylim=c(0,18),
      xlab="Elevation", ylab="lambda",lwd=2)
text(-0.9,17,"A",cex=1.5)
plot(elev, lambda, frame.plot=FALSE, ylim= c(0,38), xlab= "Elevation", ylab="")
text(-0.9,36,"B",cex=1.5)
curve(exp(beta0+ beta2*x), -1, 1, col = "red", frame.plot=FALSE, ylim= c(0,18),
      xlab= "Forest Cover", ylab= "lambda", lwd = 2)
text(-0.9, 17, "C", cex = 1.5)
plot(forest, lambda, frame.plot = FALSE, ylim=c(0,38),xlab = "Forest cover", ylab="")
text(-0.9, 36, "D", cex=1.5)


#Compute expected abundance for a grid of elevation and forest cover
cov1<- seq(-1,1,,100)
cov2<- seq(-1,1,,100)
lambda.matrix <- array(NA, dim=c(100,100))
for(i in 1:100){
  for(j in 1:100){
    lambda.matrix[i,j]<- exp(beta0+beta1*cov1[i]+beta2*cov2[j]+beta3*cov1[i]*cov2[j])
  }
}


#Interaction map of covariates
## Fix this to include interaction of Wind Speed and Elevation
par(mfrow= c(1,2), mar=c(5,4,3,2), cex.main = 1.6)
mapPalette<- colorRampPalette(c("grey","yellow","orange","red"))
image(x=cov1, y=cov2, z=lambda.matrix, col=mapPalette(100), xlab="Elevation",
      ylab="Forest cover", cex.lab=1.2)
contour(x=cov1, y=cov2, z=lambda.matrix, add=TRUE, lwd=1)
matpoints(elev, forest, pch="+", cex=0.8)


N<- rpois(n=M, lambda=lambda) #Realized abundance
sum(N) #Total pop at N sites
table(N)

mean.detection<-0.3 # Mean expected detection probability
alpha0<-qlogis(mean.detection) #Same on logit scale(intercept)
alpha1<-1 #Effect(slope) of elevation
alpha2<--3 # Effect of wind speed
alpha3<-0 #Interaction of elev and wind


logit.p<- alpha0 + alpha1*elev + alpha2*wind + alpha3*elev*wind
p<- plogis(logit.p)
mean(p)

par(mfrow=c(2,2), mar= c(5,4,2,2), cex.main=1)
curve(plogis(alpha0 + alpha1*x), -1,1, col= "red", frame.plot=FALSE, ylim=c(0,1.1),
      xlab = "Elevation", ylab = "p", lwd=2)
text(-0.9, 17, "A", cex = 1.5)
matplot(elev, p, pch="*", frame.plot=FALSE, ylim = c(0,1.1), xlab = "Elevation",
        ylab="")
text(-0.9, 17, "B", cex = 1.5)
curve(plogis(alpha0 + alpha2*x), -1,1, col= "red", frame.plot=FALSE, ylim=c(0,1.1),
     xlab = "Wind speed", ylab = "p", lwd=2)
text(-0.9, 17, "C", cex = 1.5)
matplot(wind, p, pch="*", frame.plot=FALSE, ylim = c(0,1.1), xlab = "Wind speed",
        ylab="p")
text(-0.9, 17, "D", cex = 1.5)


#Compute expected detection probability for a grid of elevation and wind speed
cov1<- seq(-1,1,,100)#elevation
cov2<- seq(-1,1,,100)#wind speed
p.matrix<- array(NA, dim= c(100,100))#Prediction matrix

for(i in 1:100){
  for(j in 1:100){
    p.matrix[i,j]<- plogis(alpha0 +alpha1*cov1[i]+alpha*cov2[j]+
                             alpha3*cov1[i]*cov2[j])
  }
}

image(x=cov1, y=cov2, z=p.matrix, col=mapPalette(100), xlab="Elevation",
      ylab="Wind speed", cex.lab=1.2)
contour(x=cov1, y=cov2, z=p.matrix, add=TRUE, lwd=1)
matpoints(elev, wind, pch="+", cex=0.7, col ="black")

#Generate counts for observation process + point counts
C<- matrix(NA, nrow=M, ncol=J)
for(i in 1:J){
  C[,i]<-rbinom(n=M, size=N, prob=p[,i])
}

head(cbind("True N" = N, "1st count" = C[,1], "2nd count" = C[,2], "3rd count" = C[,3]), 10)

table(C)

par(mfrow=c(2,2), mar= c(5,4,2,2), cex.main=1)
matplot(elev, C, pch="*", frame.plot=FALSE, ylim = c(0,40), xlab = "Elevation",
        ylab="Count(C)")
text(-0.9, 36, "A", cex = 1.5)
matplot(forest, C, pch="*", frame.plot=FALSE, ylim = c(0,40), xlab = "Forest cover",
        ylab="Count(C)")
text(-0.9, 36, "B", cex = 1.5)
matplot(wind, C, pch="*", frame.plot=FALSE, ylim = c(0,40), xlab = "Wind speed",
        ylab="Count(C)")
text(-0.9, 36, "C", cex = 1.5)
hist(C, breaks=50, col= "grey", ylim=c(0,460), main="", xlab = "Count(C)")
text(3, 450, "D", cex = 1.5)

sum(N) #True total abundance (all sites)
sum(apply(C,1,max)) #Observed abundance at all sites



### Create a function for simulation

data.fn <- function(M=267, J=3, mean.lambda=2, beta1=-2, beta2= 2, beta3= 1,
                    mean.detection=0.3, alpha1=1, alpha2=-3, alpha3=0, show.plot = TRUE){
  #create covariates
  elev<- runif(n=M, -1,1) #vector for elevation
  forest<- runif(n=M, -1,1) #vector for forest cover
  wind<- array(runif(n=M*J, -1,1), dim = c(M,J)) #array for wind speed
  #Model for abundance
  beta0<- log(mean.lambda) #Same on log scale
  lambda<-exp(beta0 + beta1*elev + beta2*forest + beta3*elev*forest)
  N<-rpois(n=M, lambda = lambda)
  Ntotal <- sum(N)
  psi.true<- mean(N>0)
  #Plots
  if(show.plot){
    par(mfrow = c(2,2), cex.main=1)
    devAskNewPage(ask = TRUE)
    curve(exp(beta0+beta1*x), -1,1, col = "red", main = "Relationship lambda-elevation\
    nat average forest cover", frame.plot=F, xlab = "Scaled elevation")
    plot(elev, lambda, xlab = "Scaled elevation", main= "Relationship lambda-elevation\
    nat average forest cover", frame.plot=F)
    curve(exp(beta0 + beta2*x), -1,1, col = "red", main = "Relationship lambda-forest\
    nat observed elevation", frame.plot=F, xlab = "Scaled forest cover")
    plot(forest, lambda, xlab = "Scaled elevation", main= "Relationship lambda-forest\
    nat observed elevation", frame.plot=F)
  }
  #Model for observations
  alpha0<- qlogis(mean.detection)
  p<- plogis(alpha0 + alpha1*elev + alpha2*wind + alpha3*elev*wind)
  C<- matrix(NA, nrow=M, ncol=J)
  for(i in 1:J){
    C[,i]<- rbinom(n=M, size=N, prob=p[,i])
  }
  summaxC<- sum(apply(C,1,max))
  psi.obs <- mean(apply(C,1,max)>0)
  
  if(show.plot){
    par(mfrow=c(2,2))
    curve(plogis(alpha0 + alpha1*x), -1,1, col = "red", main = "Relationship p-elevation\
    n at average wind speed", frame.plot=F, xlab = "Scaled elevation")
    matplot(elev, p, xlab= "Scaled elevation", main = "Relationship p-elevation\ n at 
            observed wind speed", pch="*", frame.plot=F)
    curve(plogis(alpha0 + alpha2*x), -1,1, col = "red", main = "Relationship p-wind speed\
    n at average elevation", frame.plot=F, xlab = "Scaled elevation")
    matplot(wind, p, xlab= "Scaled elevation", main = "Relationship p-wind speed\ n at 
            average elevation", pch="*", frame.plot=F)
    
    matplot(elev, C, xlab= "Scaled elevation", main = "Relationship counts and elevation",
            pch="*", frame.plot=F)
    matplot(forest, C, xlab= "Scaled forest cover", main = "Relationship counts and forest cover",
            pch="*", frame.plot=F)
    matplot(wind, C, xlab= "Scaled wind speed", main = "Relationship counts and wind speed",
            pch="*", frame.plot=F)
    desc<- paste('Counts at', M, "sites during", J, 'surveys')
    hist(C, main = desc, breaks=50, col= 'grey')
  }
  #output
  return(list(M=M,J=J, mean.lambda=mean.lambda, beta0=beta0, beta1=beta1, beta2=beta2,
              beta3=beta3, mean.detection=mean.detection, alpha0=alpha0, alpha1=alpha1,
              alpha2=alpha2, alpha3=alpha3, elev=elev, forest=forest, wind=wind, lamda=lambda,
              N=N, p=p, C=C, Ntotal=Ntotal, psi.true=psi.true, summaxC=summaxC, psi.obs=psi.obs))
}

data.fn()
data.fn(show.plot = FALSE)
data<-data.fn()
min.data<- data.fn(J=5)
min.data
###Compare observations to simulated abundances across simulations
simrep<-10000
NTOTAL<-SUMMAXC <- numeric(simrep)
for(i in 1:simrep){
  data<- data.fn(show.plot= FALSE)
  NTOTAL[i]<-data$Ntotal
  SUMMAXC[i]<-data$summaxC
}
plot(sort(NTOTAL), ylim=c(min(SUMMAXC), max(NTOTAL)), ylab="", xlab = "Simulation",
     col="red", frame=FALSE)
points(SUMMAXC[order(NTOTAL)], col="blue")
