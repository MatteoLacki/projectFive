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
		activeVariables	 <- names(activeVariables)[-1]

		transformActiveVariables <- sapply( activeVariables, function(x) paste( 'dataForModelling$',x,collapse="",sep=""),USE.NAMES=FALSE)
		
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

}

