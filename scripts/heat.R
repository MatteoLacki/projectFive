# library
require(graphics); require(grDevices)
library(plyr)

# select
names <- c("Smokes",
           "Daily_Smokes",
           "Marital_Status",
           "Age",
           "Income"
) 

# read data from Cleanup.r and clean
hdata <- Data[,names]
hdata <- hdata[!is.na(hdata$Age) & !is.na(hdata$Marital_Status),]

# income if they are a smoker or NA
incomeSmokerOrNA <- function(a, b){
  if(b == 'yes')
    a else NA
}

# label: marital status + age
getLabel <- function(text, under40){
  if(under40){
    a <- ", under 40" 
  } else {
    a <- ", 40+"
  }
  paste(text,a) 
}

# group by (marital status, age) and aggregate results
hdata2 = ddply(hdata, ~ Marital_Status ~ Age < 40,
               summarise,                
               meanIncome = mean(Income, na.rm=TRUE), 
               meanIncomeSmokers = 
                 mean(mapply(incomeSmokerOrNA, Income, Smokes), na.rm=TRUE), 
               smokers = sum(Smokes == 'yes')/length(Smokes), 
               numberOfCiggarets = 
                  sum(Daily_Smokes)/length(Smokes))
         )

# create labels
rownames(hdata2) <- mapply(getLabel, hdata2[,1], hdata2[,2]) 
hdata2[,1] <- NULL
hdata2[,1] <- NULL



#create heatmap
xh <- as.matrix(hdata2)
heatmap(t(-xh), Rowv = NA, Colv = NA, scale = "row", cexRow=1.1, cexCol=1.1, 
        margins = c(10, 16))



