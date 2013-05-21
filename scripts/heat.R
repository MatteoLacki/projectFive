# library
require(graphics); require(grDevices)
library(plyr)

# select
names <- c("Smokes",
           "Marital_Status",
           "Age_Group",
           "Smoker_Group"
) 

# read data from Cleanup.r and clean
hdata <- Data[,names]
hdata <- hdata[!is.na(hdata$Age_Group) & !is.na(hdata$Smoker_Group) & !is.na(hdata$Marital_Status),]
# join divorced and separated
hdata$Marital_Status <- factor(hdata$Marital_Status, 
                      levels = c(levels(hdata$Marital_Status), "divorced/sep."))
hdata$Marital_Status[hdata$Marital_Status == 'separated'] <- 'divorced/sep.'
hdata$Marital_Status[hdata$Marital_Status == 'divorced'] <- 'divorced/sep.'
# join 60+ into one group
hdata$Age_Group <- factor(hdata$Age_Group, 
                          levels = c(levels(hdata$Age_Group), "0-34"))
hdata$Age_Group[hdata$Age_Group == '0-24'] <- '0-34'
hdata$Age_Group[hdata$Age_Group == '25-34'] <- '0-34'
hdata$Age_Group <- factor(hdata$Age_Group, 
                          levels = c(levels(hdata$Age_Group), "60+"))
hdata$Age_Group[hdata$Age_Group == '60-64'] <- '60+'
hdata$Age_Group[hdata$Age_Group == '65+'] <- '60+'
hdata$Age_Group <- factor(hdata$Age_Group)


for(i in levels(hdata$Age_Group)){
  png(file = sprintf("heatPlot_%s.png",i),res=30,
       pointsize=50, width=800, height=500)
  
  hdata1 <- hdata[hdata$Age_Group ==i,]
  # plot by Age_group: 
  # group by marital status and aggregate results
  hdata2 = ddply(hdata1, ~ Marital_Status,
                 summarise,          
                 more_than_one_pack = sum(Smoker_Group == 'more than one pack' )
                   /length(Smoker_Group == 'more than one pack' ),
                 up_to_one_a_pack = sum(Smoker_Group == 'up to one pack' )
                   /length(Smoker_Group == 'up to one pack' ),
                 up_to_half_a_pack = sum(Smoker_Group == 'up to half a pack' )
                   /length(Smoker_Group == 'up to half a pack' ),
                 former_smoker = sum(Smoker_Group == 'former smoker' )
                   /length(Smoker_Group == 'former smoker' ),
                 never_smoked = sum(Smoker_Group == 'never smoked' )
                   /length(Smoker_Group == 'never smoked' )
 )
  
  
  #create heatmap
  rownames(hdata2) <- hdata2[,1]
  hdata2[,1] <- NULL
  xh <- as.matrix(hdata2)
  heatmap(t(-xh), Rowv = NA, Colv = NA, scale = "row", cexRow=1.1, cexCol=1.1,  margins = c(9, 11),  labRow = gsub("_", " ", rownames(t(-xh))), ylab = 'smoker group', xlab = 'marital status', labCol = c('single','married','widowed','divorced or\n separated '))
  dev.off()
} 





