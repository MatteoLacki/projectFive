#setwd("~/Documents/Scienza/Data_Mining/R_and_SAS/Progetto_V/")
#install.packages('pscl')

library(ggplot2)
library(plyr)
library(pscl)


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

formula2    <- Daily_Smokes ~ Psychiatric + Alcohol + Drugs + Criminal + Marital_Status + Employment + Age + Education + Gender

Zero_Inflated_Model	<- zeroinfl(
				formula = formula2, 
				data 	= na.omit(dataForModelling), 
				na.action = na.omit,
				dist 	= "poisson",
				link	= 'logit' )

summary(Zero_Inflated_Model)

	# When included income it gets a singularity error.


Hurdle_Model		<- hurdle(
				formula = formula2, 
				data	= dataForModelling, 
				na.action= na.omit,
				dist = "poisson", 
				zero.dist = "binomial", 
				link = "logit")


summary(Hurdle_Model)


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

Poisson_Big_Summary

individualLambdasPoisson <- as.data.frame(Poisson_Model$fitted.values)
names(individualLambdasPoisson) <- 'fitted'


goodData <- cbind(na.omit(dataForModelling), individualLambdasPoisson)


source("./scripts/funkyGorilla.R")
summary(dataForModelling)
p <- funkyGorilla(Drugs='yes', Alcohol='yes')


############################################################################################################

coefAnalysis <- matrix(nrow=28,ncol=3)


coefAnalysis[1,] <- c(modelCoefficients[3], 'no', 'Psychiatric')
coefAnalysis[2,] <- c(-modelCoefficients[3], 'yes', 'Psychiatric')


coefAnalysis[3,] <- c(modelCoefficients[4], 'no', 'Alcohol')
coefAnalysis[4,] <- c(-modelCoefficients[4], 'yes', 'Alcohol')


coefAnalysis[5,] <- c(modelCoefficients[5], 'no', 'Drugs' )
coefAnalysis[6,] <- c(-modelCoefficients[5], 'yes', 'Drugs')


coefAnalysis[7,] <- c(modelCoefficients[6], 'no', 'Criminal' )
coefAnalysis[8,] <- c(-modelCoefficients[6], 'yes', 'Criminal')


coefAnalysis[9,] <- c(modelCoefficients[7], 'married', 'Marital_Status' )
coefAnalysis[10,] <- c(modelCoefficients[8], 'widowed', 'Marital_Status' )
coefAnalysis[11,] <- c(modelCoefficients[9], 'divorced', 'Marital_Status' )
coefAnalysis[12,] <- c(modelCoefficients[10], 'separated', 'Marital_Status' )
coefAnalysis[13,] <- c(-sum(modelCoefficients[7:10]), 'single', 'Marital_Status' )


coefAnalysis[14,] <- c(modelCoefficients[11], 'private sector', 'Employment' )
coefAnalysis[15,] <- c(modelCoefficients[12], 'enterpreneur', 'Employment' )
coefAnalysis[16,] <- c(modelCoefficients[13], 'farmer', 'Employment' )

coefAnalysis[17,] <- c(modelCoefficients[14], 'pensioner', 'Employment' )
coefAnalysis[18,] <- c(modelCoefficients[15], 'retiree', 'Employment' )
coefAnalysis[19,] <- c(modelCoefficients[16], 'pupil or student', 'Employment' )

coefAnalysis[20,] <- c(modelCoefficients[17], 'unemployed', 'Employment' )
coefAnalysis[21,] <- c(modelCoefficients[18], 'other non-active', 'Employment' )
coefAnalysis[22,] <- c(-sum(modelCoefficients[11:18]), 'civil servant', 'Employment' )

coefAnalysis[23,] <- c(modelCoefficients[20], 'technical', 'Education' )
coefAnalysis[24,] <- c(modelCoefficients[21], 'secondary', 'Education' )
coefAnalysis[25,] <- c(modelCoefficients[22], 'beyond secondary', 'Education' )
coefAnalysis[26,] <- c(-sum(modelCoefficients[20:22]), 'primary or less', 'Education' )

coefAnalysis[27,] <- c(modelCoefficients[23], 'female', 'Gender')
coefAnalysis[28,] <- c(-modelCoefficients[23], 'male', 'Gender')

tmp<-as.numeric( coefAnalysis[,1] ) 
coefAnalysis 	<- as.data.frame( coefAnalysis )
names(coefAnalysis) <- c('Marginal_Effect', 'Response', 'Discrete_Variables')
coefAnalysis[,'Marginal_Effect'] <- tmp

rm(tmp)
############################################################################################################
vp.setup <- function(x,y)
{
	 grid.newpage()
	 # define viewports and assign it to grid layout
	 pushViewport(viewport(layout = grid.layout(x,y)))
}

vp.layout <- function(x,y){
viewport(layout.pos.row=x, layout.pos.col=y)
}

attach(coefAnalysis)

M1 <- coefAnalysis[Discrete_Variables=='Psychiatric',1:2]


t1 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Psychiatric',1:2], geom="bar",fill=I(100), stat="identity") + ylim(-1, 1) + labs(x='',y='Marginal Effect',title = "Psychiatric")+ opts(axis.text.x=theme_text(angle=90))
t2 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Alcohol',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='', title = "Alcohol")+ opts(axis.text.x=theme_text(angle=90))
t3 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Drugs',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='',title = "Drugs")+ opts(axis.text.x=theme_text(angle=90))
t4 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Criminal',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='', title = "Criminal")+ opts(axis.text.x=theme_text(angle=90))
t5 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Marital_Status',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='Marginal Effect',title = "Marital Status")+ opts(axis.text.x=theme_text(angle=90))
t6 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Employment',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='', title = "Employment")+ opts(axis.text.x=theme_text(angle=90))
t7 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Education',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='', title = "Education")+ opts(axis.text.x=theme_text(angle=90))
t8 <- qplot(Response, Marginal_Effect, data=coefAnalysis[Discrete_Variables=='Gender',1:2], geom="bar",fill=I(100)) + ylim(-1, 1) + labs(x='',y='', title = "Gender")+ opts(axis.text.x=theme_text(angle=90))


vp.setup(2,4)

print(t1, vp=vp.layout(1,1))
print(t2, vp=vp.layout(1,2))
print(t3, vp=vp.layout(1,3))
print(t4, vp=vp.layout(1,4))
print(t5, vp=vp.layout(2,1))
print(t6, vp=vp.layout(2,2))
print(t7, vp=vp.layout(2,3))
print(t8, vp=vp.layout(2,4))

detach(coefAnalysis)
############################################################################################################
rm( Relative_Binary_Change )
rm( Poisson_Model )
rm(contrasts, dataForModelling, variables, variablesForModelling)