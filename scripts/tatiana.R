library(ggplot2)
library(plyr)

attach(Data)


Data<- Data[Data$Income < 10000,]

qplot(Income, ..density.., data=Data2, geom="density", fill=Smoker_Group, position="stack") +
   coord_flip()





ggplot(data=Data,aes(x=Income,fill=Smoker_Group)) + 
  geom_bar(subset=.(Gender=="F")) + 
  geom_bar(subset=.(Gender=="M"),aes(y=..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()

summary(Income)
