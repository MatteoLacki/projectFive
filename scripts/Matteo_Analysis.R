#setwd("~/Documents/Scienza/Data_Mining/R_and_SAS/Progetto_V/")

load("data/Data.RData")
summary(Data)

variables 		<- colnames(Data)		
variablesForModelling 	<- setdiff( variables, c("Smokes", "Ever_Smoked", "Age_Group", "Smoker_Group"))

dataForModelling 	<- Data[, variablesForModelling]
summary(dataForModelling)

rm(Data)

#attach(dataForModelling)
summary(dataForModelling)



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


PoissonProbabilityFunction <- function( countedThing, regressants, coefficients ) 
{
	if ( countedThing < 0 ) 		stop("Only non-negative values.")
	else if ( countedThing %%1 != 0 )  	stop("Only natural values.")
	else 
	{
		lambda	<- exp( crossprod(regressants, coefficients) )	
		return( exp( - lamda + countedThing*log(lambda) - sum(log(1:countedThing)) ) )
	} 
}


Poisson_Big_Summary 	<- summary( Poisson_Model )
Poisson_Big_Summary

modelCoefficients 	<- Poisson_Big_Summary$coefficients[,1]
modelCoefficients
	
z <- list(a = "agg")
z$a
	# Black-and-white histograms created for all possible variables
library(ggplot2)

Not_Filled_Histograms <- 
	lapply(
		variables,
		function(x)
		{ 
			qplot( 
				eval(parse(text = x)), 
				data = Data, 
				geom="histogram", 
				ylab="No of people", 
				xlab=gsub("_", " ", x)
			) 
		}	
	)
names(Not_Filled_Histograms) <- variables
variables

Not_Filled_Histograms$Daily_Smokes





datach(dataForModelling)
rm(contrasts, dataForModelling, variables, variablesForModelling)




	# Making all different histograms that are filled with third variable.
Filled_Histograms	<- HIST_LIST_UNWRAPPED(Data)


Poisson_Model 		<- glm( Cigarettes_Daily ~ ., data=Data_for_Modelling, family=poisson)
Poisson_Big_Summary 	<- summary( Poisson_Model )

Poisson_Summary		<- TRANSLATE_RESULTS( Poisson_Big_Summary$coefficients )

Relative_Binary_Change 	<- as.matrix( sapply( Poisson_Summary[,1], RELATIVE_CHANGE), nrows = nrow(Poisson_Summary), ncol=1 )
colnames(Relative_Binary_Change) <- "Effects"

Poisson_Summary		<- cbind(Poisson_Summary, Relative_Binary_Change)

rm( Relative_Binary_Change )
rm( Poisson_Model )



# Make it nice.. and add another type of model.
	
#align( Poisson_Summary ) <- "lcccc"

