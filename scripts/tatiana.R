library(ggplot2)
library(plyr)



attach(Data)


summary(Income)

# usuwa te wiersze, w których wszystkie istotne kolumny są NA
# Przygotowuje data.frame wypełniony TRUE i NA. 

Data_temp <- Data[!is.na(Data$Income),]
summary(Data_temp$Income)
#



#Drawing Histogram of Incomes divided be Genders. Upper Income is limited by 10,000 PLN
#######################################################################################

tp1 <- histogram(~ Income | factor(Gender), data = Data_temp[Data_temp$Income < 10000, ])

lines(density(~ Income | factor(Gender), data = Data_temp[Data_temp$Income < 10000,], bw=3))  # plots density graph over histogram and function
rug(density(~ Income | factor(Gender), data = Data_temp[Data_temp$Income < 10000,], bw=0.3))    #show the actual data points

summary(tp1)
plot(tp1, split = c(1, 1, 1, 1))





tp2 <- densityplot(~ Daily_Smokes, data = Data_temp[Data_temp$Daily_Smokes > 0, ], groups = Gender,
                   plot.points = FALSE,
                   type = c("g"),
                   pch  = 10,
                   auto.key = list(space = "right", title = "Gender"))
class(tp2)
plot(tp2, split = c(1, 1, 1, 1), newpage = TRUE)






qplot(Income, ..density.., data=Data_temp[Data_temp$Income < 10000,], geom="density", fill=Smoker_Group, position="stack") +
  scale_fill_brewer(palette=6, name="Smoking level") 




qplot(Income, ..density.., data=Data_temp[Data_temp$Income < 10000,], geom="density", aes(depth), fill=Smoker_Group, position="stack") +
  coord_flip() 
  








#    Dalej nie dziala
##################################################################################################

# Zrobmys factory dla vektoru Income

Income_factor <- factor(Income)
levels(Income_factor)
Income_factor
#


qplot(Income, ..density.., data=Data_temp[Data_temp$Income < 10000,], geom="density", fill=Smoker_Group, position="stack") +
  scale_fill_brewer(palette=9, name="Smoking level") + 
  geom_density(subset = .(Gender == "F")) + 
  geom_density(subset = .(Gender == "M"),aes(y = ..count..*(-1))) + 
  coord_flip()





ggplot(data=Data_temp,aes(x = Data_temp[Data_temp$Income < 10000,], fill = Smoker_Group)) +
  geom_density(subset = .(Gender == "F")) + 
  geom_density(subset = .(Gender == "M"),aes(y = ..count..*(-1))) + 
  scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()


