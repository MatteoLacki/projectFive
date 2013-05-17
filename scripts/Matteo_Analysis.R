setwd("~/Documents/Scienza/Data_Mining/R_and_SAS/Progetto_V/")

load("data/Data.RData")
ls()

variables <- colnames(Data)
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

dataForModelling 	<- Data[, setdiff( variables, c("Smokes", "Ever_Smoked", "Age_Group", "Smoker_Group"))]

summary(dataForModelling)

Poisson_Model 		<- glm( Daily_Smokes ~ ., data=dataForModelling, family=poisson)
Poisson_Big_Summary 	<- summary( Poisson_Model )
Poisson_Big_Summary




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

