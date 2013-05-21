library(ggplot2)

funkyGorilla <- function(	Psychiatric=NA, 
				Alcohol=NA, 
				Drugs=NA, 
				Criminal=NA, 
				Marital_Status=NA, 
				Employment=NA, 
				Education=NA, 
				Gender=NA)
{
	activeVariables  <- as.list(match.call())
	if (!is.null(names(activeVariables)))
	{
		activeVariables	 	<- names(activeVariables)[-1]

		noOfActiveVariables	<- length(activeVariables)

		nonActiveVariables	<- 
			setdiff(variablesForModelling, activeVariables)		
			
		transformActiveVariables <- sapply( activeVariables, function(x) paste( 'dataForModelling$',x,collapse="",sep=""),USE.NAMES=FALSE)
		
		activeVariablesValues	<- sapply(activeVariables, function(x) eval(parse(text = x)),USE.NAMES=FALSE )

		tmpVal	<- 
			sapply( 	
				activeVariables, 
				function(x) paste(	'\"',
							 eval(parse(text = x)),
							'\"', 
							collapse="",
							sep=""),
				USE.NAMES=FALSE 
			)


		M <- rbind(transformActiveVariables, tmpVal)
		M <- apply(M, 2,  function(x){ paste(x, sep='', collapse='==') })
		M <- paste(M, sep='', collapse=' & ')
		

			# Data restricted to values passed to function.
		selectedData <- na.omit(dataForModelling[eval(parse(text=M)),])

		result <- 
		qplot(  
			Daily_Smokes, 
			data = selectedData, 
			geom="histogram", 
			ylab="No of people", 
			xlab='Cigarettes smoked daily',
			binwidth=1,
			fill=I(4)
		)
		
			# The model uses na.omit in standard.

		A <- na.omit(	count(	selectedData,
					nonActiveVariables )	)

		noOfRestrictedOccurences <- nrow(A)

			# Normalisation
		A$freq <- A$freq/noOfRestrictedOccurences
	

		B <- as.data.frame(	matrix(	rep.int(	activeVariablesValues, 
								times=noOfRestrictedOccurences ), 
						nrow=noOfRestrictedOccurences, 
						ncol=noOfActiveVariables, 
						byrow=TRUE ) 	)

		names(B) 	<- activeVariables

		A <- cbind(B,A)

		A <- merge(A, goodData )		

		A <- A[,c('freq','fitted')]

		frequency 	<- A[,'freq']		
		fitted		<- A[,'fitted']

		
		upperlimit  <- max(selectedData$Daily_Smokes)
				
		partialDistributions <- 
			t(
				sapply(
					fitted, 
					function(x) generatingTheoreticalDistributionValues(
						x, 
						upperlimit
						)
				)
			)
				
		theoreticalValues <- t(crossprod(frequency, partialDistributions ))
		theoreticalCounts <- theoreticalValues*noOfRestrictedOccurences		


		theoreticalCounts <- as.data.frame(cbind(0:upperlimit, theoreticalCounts))
		
		names(theoreticalCounts) <- c('cigarettes', 'theoretic_counts' )
	
		
		result <- result + 
			geom_point(	data=theoreticalCounts, 
					aes(x=cigarettes,y=theoretic_counts),
					colour = I("red"), 
					size = I(3)
			)

		

	} else 
	{
		result <- qplot(  
			Daily_Smokes, 
			data = dataForModelling, 
			geom="histogram", 
			ylab="No of people", 
			xlab='Cigarettes smoked daily',
			binwidth=1,
			fill=I(4)
		)
	}

	return(result)
}

