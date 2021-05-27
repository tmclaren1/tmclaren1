###Simulation for count data
###attempt to adapt to CLNU data
library(MASS)

n.trnscts.per.Forest_type <- 3
n.vsts.per.month <- 2

data.CLNU <- function(M=30, J=10, mean.lambda=18, beta1=-2, beta2= 2, beta3= 1,
                    mean.detection=0.1, alpha1=0, alpha2=0, alpha3=0, show.plot = TRUE){
  #create covariates
  elev<- runif(n=M, -1,1) #vector for elevation
  forest<- runif(n=M, -1,1) #vector for forest cover
  wind<- array(runif(n=M*J, -1,1), dim = c(M,J)) #array for wind speed
  #new covariates
  new_FT_sim <- rep(c("PIAL", "PICO", "PIFL", "PSME","PIEN"),
                         each = n.trnscts.per.Forest_type)
  new_mo_sim <- rep(c("July-August", "October", "September"),
                   each = n.vsts.per.month)
  
  #Model for abundance
  beta0<- log(mean.lambda) #Same on log scale
  lambda<-exp(beta0 + beta1*elev + beta2*forest + beta3*elev*forest)
  N<-rnegbin(n=M, mu = lambda, theta = 1.6)
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
###
### M=11
### J=4
### Param dist = neg binom
### Cofactors: Forest type: 5 levels
###            Month: 3 levels
### Covariates: Year: -0.05 per year




data.CLNU()
data.CLNU(M=11, J=4, mean.lambda = 18.7, mean.detection = 0.1)
data<-data.CLNU(show.plot = FALSE)

data$C



