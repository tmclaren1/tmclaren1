library(unmarked)
library(rjags)
library(fastDummies)

Forests<-dummy_cols(x$Forest_type)


dmax=0.35 #max. detection distance
db <- c(0.0,0.17,0.25,0.35)  #distance bins
point.area<- pi*0.35*0.35 #area of point count survey in ha


## get coefficient estimates to be used in data simulation
beta<-coef(gdist1nb)
beta
betaFall<-beta[c("lambda(Int)")]
betaFall

##predict expected abundance per point count on log-scale
Xspr<-cbind(rep(1,65))
lam<-Xspr%*%(betaFall)


#get parameters for detection function
dparm<-beta[c("p(Int)")]
sigma<-exp(Xspr%*%dparm)

J<-30 #number of sampling points

##########################################################################################################################################
##### stuff to set for the different scenarios #########

### year effect ###
yrs<-0:14 #six consecutive surveys
nyrs<-length(yrs)

lamnew<-exp(lam)

#change recruitment to .3 for pop. rate of change of .9
surv=0.6
rec=0.35  #set to 0.3 for pop. rate of change of 0.95
Beta<- log(surv+rec)

#### number of sampling points ### 

npoints<- J #set to 200 or 100 for reduced sample
Yr<-rep(yrs, each=npoints)

nsim=100

##### distance category info
nG<-length(db)-1
maxd<-dmax
delta=c(0.17,0.125,0.25)####Not sure if this is supposed to be total diameter of circle in each band
xg<-c(0.085, 0.21, 0.3)
pix<-(2*xg*delta )/(maxd*maxd) ###Find out how this equation works


#we need this because otherwise JAGS cannot build a sampler, recommended by M. Plummer
set.factory("bugs::Conjugate", FALSE, type="sampler")


### begin sim loop

for (i in 1:nsim) {
  print(i)
  
  ### dsim.fun at the end of this script #####
  dat<-dsim.fun(dmax=0.35, lamnew=lamnew, sigma=sigma, surv=surv, rec=rec, npoints=npoints)
  
  y<-sapply(dat$detList, rowSums)
  
  ### get one long vector with detections, distance category and site across all years
  dclass<-site<-NULL
  
  for (t in 1:nyrs){
    for (j in 1:npoints){
      if (y[j,t]==0) next
      
      ssi<-rep(j, y[j,t])
      
      dc<-NULL
      for (k in 1:nG){
        if(dat$detList[[t]][j,k]==0) next
        dd<-rep(k, dat$detList[[t]][j,k])
        dc<-c(dc, dd)
      }
      
      dclass<-c(dclass, dc)
      site<-c(site, ssi)
    }
  }
  
  yin<-y+1 #this is for trend model
  
  data1<-list(nsites=npoints, chap=as.vector(covs$chap)[dat$cell], chap2=as.vector(covs$chap^2)[dat$cell], elev=as.vector(covs$elev)[dat$cell],
              T=nyrs,nG=nG, xg=xg, pi=pix, y=y,dclass=dclass, site=site, nind=sum(y))
  
  inits<-function(){list(N=yin,alpha=runif(1), beta1=runif(1), beta2=runif(1), beta3=runif(1),
                         alphaS=runif(1,3,5), betaS1=runif(1),gamma=runif(1,0.6,0.99))} 
  
  params<-c('alpha','beta1','beta2','beta3','alphaS','betaS1','gamma', 'rout')#
  
  mod<-jags.model("Power_sim_model.R", data1, inits, n.chains = 3, n.adapt=500)
  out<-coda.samples(mod, params,thin=10, n.iter=30000)
  res<-summary(window(out, start=1001))
  
  if(is.numeric(try(max(gelman.diag(out)[[1]][,1]), silent=T))) Rmax<-max(gelman.diag(out)[[1]][,1]) else Rmax<-NA
  
  
  dput(     list(data=dat, model=res,R=Rmax, mcs=out),
            file=paste("MarcovData_IndModel_", npoints, "_", round(Beta, dig=2), "decline_", i,".R", sep='')         )
  
}  #end of sim loop 



###############################################################################################################################################
####### summarize simulation results ##########################################################################################################


###function to exponentiate all posterior samples of beta4, so that it is on the same scale as
###the population rate of change lambda in the Markov model

b4tolam<-function(x){
  lam.mcmc<-list()
  for (j in 1:3){
    lam.mcmc[[j]]<-exp(x[[j]][,dimnames(x[[1]])[[2]] == "beta4" ])
  }
  return(lam.mcmc)
}


str<-c(paste("MarcovData_IndModel_", npoints, "_", round(Beta, dig=2), "decline_", 1:nsim,".R", sep=''))


truebeta<-c(beta[1:4], Beta, beta[5:7]) 
BetaRes<-array(0, c(nsim, 6, 8))
dimnames(BetaRes)<-list((1:nsim), c('Mean','p', 'Bias','Lower','Upper', 'Coverage'),
                        c("lambda(Int)","lambda(chap)","lambda(I(chap^2))","lambda(elev)", "exp(lambda(year))", "p(Int)", "p(chap)", "alpha(alpha)") )


for (i in 1:100) {

out<-source(str[i])

mcm<-out$value$mcs
lam<-b4tolam(mcm)
sum.lam<-summary(mcmc.list(lam))

mod<-out$value$model
mod$statistics["beta4",]<-sum.lam$statistics
mod$quantiles["beta4",]<-sum.lam$quantiles

coefs<-mod$statistics[,1]
ord<-c(1,3,4,5,6,2,7,8)  
CI<-mod$quantiles[ord,c(1,5)]

#coef values
BetaRes[i,1,]<-coefs[ord]

#bias
BetaRes[i,3,]<-(coefs[ord]-truebeta)/truebeta

#lower CI
BetaRes[i,4,]<- CI[,1]

#upper CI
BetaRes[i,5,]<- CI[,2]

#coverage, p-value
for ( k in 1:length(truebeta)) {
if (truebeta[k] >= BetaRes[i,4,k] & truebeta[k] <= BetaRes[i,5,k] ) BetaRes[i,6,k]<-1 else BetaRes[i,6,k]<-0
if (k==5) next
if(CI[k,1]<0 & CI[k,2]>0)    BetaRes[i,2,k]<-0 else  BetaRes[i,2,k]<-1
}

#p-value for exp(beta4)
if(CI[5,2]<1)    BetaRes[i,2,5]<-1 else  BetaRes[i,2,5]<-0

}

Restab=matrix(0, nrow=8, ncol=5)
colnames(Restab)=c('Mean','rmse','BiasMean','CI', 'significance')
rownames(Restab)=c("lambda(Int)","lambda(chap)","lambda(I(chap^2))","lambda(elev)", "exp(lambda(year))", "p(Int)", "p(chap)", "alpha(alpha)") 

#means
Restab[,1]<-apply(BetaRes[,1,], 2, mean)

#rmse
for (k in 1:8){
intm<-sqrt(sum((BetaRes[,1,k]-truebeta[k])^2)/nsim)
Restab[k,2]<-intm
}

#bias
Restab[,3]<-apply(BetaRes[,3,],2,mean)

#Coverage
Restab[,4]<-apply(BetaRes[,6,],2,sum)

#significance
Restab[,5]<-apply(BetaRes[,2,],2,sum)





#################################################################################################################
###### function to generate distance sampling data ##############################################################

dsim.fun<-function(dmax=0.35, lamnew=lamnew,sigma=sigma,surv=surv, rec=rec, npoints=npoints){

J=dim(lamnew)[1] #number of grid cells
nyrs=15

cell<-sort(sample(1:J, npoints, replace=FALSE))

Nsim<-matrix(nrow=npoints, ncol=nyrs)

#yr 1 as before
Nsim[,1]<-rnbinom (n=npoints, size=exp(-1.02), mu=lamnew[cell,1]) #generate individual countes per grid cell/point count circle

for(y in 2:nyrs){
Nsim[,y]<-rbinom(npoints,Nsim[,y-1], surv) + rpois(npoints,Nsim[,y-1]*rec)
}

#generate distance from hypothetical point count locations 

#first set prob for an individual to be in a 1-m distance class from the center point

rc<-1:dmax
ri<-(0:(dmax-1))
ar<-NULL
for (i in 1:dmax) {
ar[i]<-pi * (rc[i]^2 - ri[i]^2)
}
pcc<-ar/sum(ar)

Nc<-matrix(nrow=0, ncol=2)
NcList<-list(Nc,Nc,Nc,Nc,Nc,Nc) 

for (y in 1:nyrs){
for (j in 1:J){
if (Nsim[j,y]==0) next
junk<-rmultinom(1, Nsim[j,y],pcc)
tt<-rep( (which(junk!=0) - 0.5), (junk[which(junk!=0)]) ) 
Ndist<-cbind(rep(j,Nsim[j,y]),tt )
NcList[[y]]<-rbind(NcList[[y]], Ndist)
}}

# for each sampling point generate detection data based on distance of individuals within a max of 300m
# and the detection model from the paper

detec<-NULL
detList<-list(detec, detec,detec,detec,detec,detec)

for (y in 1:nyrs){
for (j in 1:J) {
dvec<-NcList[[y]][ which(NcList[[y]][,1]==j)  ,2 ]

if(length(dvec)==0) {
det<-c(rep(0,3))
} else {
pvec<-exp(-dvec*dvec/(2*(sigma[j]^2)))
dets<-dvec[rbinom(length(dvec),1,pvec )==1]
det<-table(cut(dets, db, include.lowest=T))
}
detList[[y]]<-rbind(detList[[y]],det)
}
}

return(list(NcList=NcList, detList=detList, N=Nsim, cell=cell)) 

}#end function















