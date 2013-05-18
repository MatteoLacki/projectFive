setwd("~/Documents/Scienza/Data_Mining/R_and_SAS/Progetto_V/")

load("data/Data.RData")
ls()

variables 		<- colnames(Data)		
variablesForModelling 	<- setdiff( variables, c("Smokes", "Ever_Smoked", "Age_Group", "Smoker_Group"))

summary(Data)
head(Data$Smokes)

is.ordered(Data$Smokes)

is.ordered(Data$Daily_Smokes)
is.ordered(Data$Ever_Smoked)
is.ordered(Data$Psychatric)
is.ordered(Data$Alcohol)
is.ordered(Data$Drugs)
is.ordered(Data$Criminal)
is.ordered(Data$Marital_Status)

is.ordered(Data$Age)
is.ordered(Data$Age_Group)
is.ordered(Data$Employment)
is.ordered(Data$Gender)
is.ordered(Data$Smoker_Group)
head(Data$Smoker_Group)

dataForModelling 	<- Data[, variablesForModelling]

attach(dataForModelling)

is.ordered(Employment)
dataForModelling$Employment <- factor(unclass(Employment)) 
levels(dataForModelling$Employment) <- c("civil servant", "private sector", "entrepreneur", 
                    "farmer", "pensioner","retiree", "pupil or student", 
                    "unemployed", "other non-active")

head(Data$Employment)
head(dataForModelling$Employment)

summary(dataForModelling)

Poisson_Model 		<- glm( Daily_Smokes ~ ., data=dataForModelling, family=poisson, contrasts = contrastsTest)

variablesForModelling


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
			Education= 	"contr.helmert",	
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

	
z <- list(a = "agg")
z$a
	# Black-and-white histograms created for all possible variables

Not_Filled_Histograms <- 
	lapply(
		Names_of_Variables_Polish, 
		function(x)
		{ 
			qplot( 
				eval(parse(text = x)), 
				data = Dane, 
				geom="histogram", 
				ylab="No of people", 
				xlab=gsub("_", " ", x)
			) + coord_flip()
		}	
	)


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

