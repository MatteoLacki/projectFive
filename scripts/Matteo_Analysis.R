#setwd("~/Documents/Scienza/Data_Mining/R_and_SAS/Progetto_V/")

library(ggplot2)
library(plyr)

load("data/Data.RData")

variables 		<- colnames(Data)		
variablesForModelling 	<- setdiff( variables, c("Smokes", "Ever_Smoked", "Age_Group", "Smoker_Group"))
regressors		<- setdiff( variablesForModelling, "Daily_Smokes")
nonContinuousRegressors <- c('Psychiatric', 'Alcohol', 'Drugs', 'Criminal', 'Marital_Status', 'Employment', 'Education', 'Gender')

dataForModelling 	<- Data[, variablesForModelling]

rm(Data)


			# What is that? Contrasts.. Take a look here
		# http://www.unc.edu/courses/2007spring/biol/145/001/docs/lectures/Nov26.html
		# http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm

formula	  <- Daily_Smokes ~ Income + Psychiatric + Alcohol + Drugs + Criminal + Marital_Status + Employment + Age + Education + Gender

contrasts <- 	list(Psychiatric= 	"contr.treatment", 
			Alcohol= 	"contr.treatment",
			Drugs= 		"contr.treatment", 
			Criminal= 	"contr.treatment",
			Marital_Status= "contr.treatment",
			Employment= 	"contr.treatment",	
			#Education= 	"contr.helmert",
			Education= 	"contr.treatment",	
			Gender= 	"contr.treatment" )



Poisson_Model 		<- glm( formula	= formula, 
				data	= dataForModelling, 
				family	= poisson, 
				contrasts = contrasts )


PoissonProbabilityFunction <- function( noOfOccurences, lambda ) 
{
	if ( noOfOccurences < 0 ) 		stop("Only non-negative values.")
	else if ( noOfOccurences %%1 != 0 )  	stop("Only natural values.")
	else 
	{
		return( 
			ifelse( noOfOccurences==0, 
				exp(-lambda),
				exp( - lambda + noOfOccurences*log(lambda) - sum(log(1:noOfOccurences)) )
			) 
		)
	} 
}


generatingTheoreticalDistributionValues <- function( lambda, upperlimit )
{
	return( sapply(0:upperlimit, function(x) PoissonProbabilityFunction(x, lambda) ) )
}


Poisson_Big_Summary 	<- summary( Poisson_Model )
modelCoefficients 	<- Poisson_Big_Summary$coefficients[,1]

individualLambdasPoisson <- as.data.frame(Poisson_Model$fitted.values)
names(individualLambdasPoisson) <- 'fitted'


#goodData <- cbind(na.omit(dataForModelling), Poisson_Model$fitted.values)
goodData <- cbind(na.omit(dataForModelling), individualLambdasPoisson)





Relative_Binary_Change 	<- as.matrix( sapply( Poisson_Summary[,1], RELATIVE_CHANGE), nrows = nrow(Poisson_Summary), ncol=1 )
colnames(Relative_Binary_Change) <- "Effects"
Poisson_Summary		<- cbind(Poisson_Summary, Relative_Binary_Change)

source("./scripts/funkyGorilla.R")
p <- funkyGorilla(Drugs='yes', Alcohol='yes')
p



rm( Relative_Binary_Change )
rm( Poisson_Model )
rm(contrasts, dataForModelling, variables, variablesForModelling)



# Make it nice.. and add another type of model.
	
#align( Poisson_Summary ) <- "lcccc"

