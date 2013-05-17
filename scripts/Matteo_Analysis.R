	### Data Manipulation and Modelling
	### Mateusz ????cki
	
Names_of_Variables_Polish	<- colnames(Dane)
No_of_Variables			<- length( colnames(Dane) )


	### Translating column names into English.

Data 				<- Dane
colnames(Data) 			<- DICTIONARY_POLISH_ENGLISH(colnames(Dane))
Names_of_Variables_English 	<- colnames(Data)
Data_for_Modelling		<- Data[,-1] 	### We get rid of the do-you-smoke question. 

	# Resetting the factors
	
levels(Data$Education) <- c("bez wyksztalcenia","podstawowe ukonczone", "policealne", "srednie ogolnoksztalcace","srednie zawodowe","wyzsze ze stopniem inzyniera, licencjata","wyzsze ze stopniem magistra lub rownorzednym","wyzsze ze stopniem naukowym co najmniej doktora","zasadnicze zawodowe")

	# Translation of tuples - it's not vectorised, but I lost about 5 hours trying to do it
	
for( i in 1:length(colnames(Data)))
{
	levels(Data[,i]) 	<- as.vector(sapply(levels(Data[,i]), DICTIONARY_POLISH_ENGLISH))
}
	
rm(i)	


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

