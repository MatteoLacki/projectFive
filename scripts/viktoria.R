library(ggplot2)
library(plyr)
# the function below describes size of each smoker group depending of gender
gender <- function(Data, grouping_variable, gender_variable) {
  Smoker_Group <- which(colnames(Data) == grouping_variable)
  Gender <- which(colnames(Data) == gender_variable)
  
  data_grouped <- ddply(Data, .variables = c(Smoker_Group, Gender), .fun = nrow)
  
  ggplot(data_grouped, aes(x = Smoker_Group, y = V1, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = V1), vjust = -0.5, colour = "black",
              position = position_dodge(.9), size = 4) +
    scale_fill_manual(values=c("#E69F00", "#009E73")) +
    ylab("Quantity") +
    ggtitle("Smoker groups depending on gender")
}

# the function below describes distribution of the smoker groups for different ages
age <- function(Data, grouping_variable, age_variable) {
  Smoker_Group <- which(colnames(Data) == grouping_variable)
  Age <- which(colnames(Data) == age_variable)  
  prop <- ddply(Data, .variables = c(Age, Smoker_Group), .fun = function(x){
    (nrow(x)/nrow(Data[which(Data$Age == x$Age[1]),]))*100
  })
  
  ggplot(prop, aes(x = Age, y = V1, fill = Smoker_Group)) +
    geom_area(colour = "black", size = .2, alpha = .4) +
    scale_fill_brewer(palette = "Oranges", breaks = rev(levels(Data$Smoker_Group))) +
    scale_y_reverse() +
    ylab("Percentage of smokers in age group") +
    ggtitle("Distribution of smokers depending on age") +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}  

gender(Data, "Smoker_Group", "Gender")
age(Data, "Smoker_Group", "Age")


