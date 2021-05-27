library(ggplot2)
ISSJ<-as.data.frame(Xall.100m)



sumdata=data.frame(value=apply(ISSJ,2,sum))
sumdata$key=rownames(sumdata)

ggplot(data=sumdata, aes(x=key, y=value)) +
  geom_bar(colour="black", stat="identity")
