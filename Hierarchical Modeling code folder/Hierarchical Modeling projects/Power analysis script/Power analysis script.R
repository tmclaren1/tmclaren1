##power analysis simulation
library(ggplot2)
library(plyr)

ctrl<-rnorm(100,5,sd =2)
trt<-rnorm(100,7,sd =2)

hist(ctrl)
hist(trt)

###first groups
df <- data.frame(
  group=factor(rep(c("control", "treatment"), each=200)),
  measure=(c(rnorm(200, mean=5, sd=3), rnorm(200, mean=15, sd=3)))
)

df
mu <- ddply(df, "group", summarise, grp.mean=mean(measure))

ggplot(df, aes(x=measure, fill=group)) +
  geom_histogram( alpha=0.5, position="identity")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme_bw()


### Second groups: smaller difference in means
df1 <- data.frame(
  group=factor(rep(c("control", "treatment"), each=200)),
  measure=(c(rnorm(200, mean=5, sd=3), rnorm(200, mean=10, sd=3)))
)

df1
mu1 <- ddply(df1, "group", summarise, grp.mean=mean(measure))

ggplot(df1, aes(x=measure, fill=group)) +
  geom_histogram( alpha=0.5, position="identity")+
  geom_vline(data=mu1, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme_bw()

### third groups: original difference in means but wider sd
df2 <- data.frame(
  group=factor(rep(c("control", "treatment"), each=200)),
  measure=(c(rnorm(200, mean=5, sd=4), rnorm(200, mean=15, sd=3)))
)

df2
mu2 <- ddply(df2, "group", summarise, grp.mean=mean(measure))

ggplot(df2, aes(x=measure, fill=group)) +
  geom_histogram( alpha=0.5, position="identity")+
  geom_vline(data=mu2, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme_bw()

###fourth groups: smaller data size
df3 <- data.frame(
  group=factor(rep(c("control", "treatment"), each=20)),
  measure=(c(rnorm(20, mean=5, sd=3), rnorm(20, mean=15, sd=3)))
)

df3
mu3 <- ddply(df3, "group", summarise, grp.mean=mean(measure))

ggplot(df3, aes(x=measure, fill=group)) +
  geom_histogram( alpha=0.5, position="identity")+
  geom_vline(data=mu3, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme_bw()

t.test(measure~group, data=df3)

###fifth groups:even smaller sample size and larger sd
df4 <- data.frame(
  group=factor(rep(c("control", "treatment"), each=100)),
  measure=(c(rnorm(100, mean=5, sd=3), rnorm(100, mean=7, sd=3)))
)

df4
mu4 <- ddply(df4, "group", summarise, grp.mean=mean(measure))

ggplot(df4, aes(x=measure, fill=group)) +
  geom_histogram( alpha=0.5, position="identity")+
  geom_vline(data=mu4, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme_bw()

t.test(measure~group, data=df4)


### Time series simulation

e <- rnorm(500)
f <- rnorm(500, 1)
m1 <- arima.sim(model = list(c(ma=0.8,alpha=1,beta=10)),n=500)

plot(m1)

e <- rnorm(500,mean=0,sd=1)

##Take 2
t <- 1:50
alpha <- 15
beta <- -0.03
theta <- 1
ts <- alpha + beta * t + arima.sim(list(ma = theta), n = length(t))

plot(ts)

t1<-as.vector(t)
ts.df<-as.data.frame(cbind(t1,ts))
plot(ts.df)
ts.lm<-(lm(ts~t1, data=ts.df))


plot(ts, xlab = "Year", ylab = "population index")
abline(ts.lm)

newx<-seq(min(ts.df$t1),max(ts.df$t1),1)
b<-predict(ts.lm,newdata=data.frame(t1=newx),interval="confidence")
plot(ts, xlab = "Year", ylab = "population index")
abline(ts.lm)
lines(newx,b[,2],lty=1,col="blue", lwd = 2)
lines(newx,b[,3],lty=1,col="blue", lwd = 2)


##Take 3
alpha <- 1
beta <- 0
theta <- 0.8
m_1 <- 0
for(i in 2:length(e)){
  m_1[i] <- alpha+beta*i+theta*m_1[i-1]+e[i]
}
plot(m_1)
