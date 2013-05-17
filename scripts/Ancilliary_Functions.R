VP_SETUP <- function(x,y)
{
		# create a new layout with grid
	grid.newpage()
	
		 # define viewports and assign it to grid layout
	
	pushViewport(viewport(layout = grid.layout(x,y)))
}

#############################################################################################3

# define function to easily access layout (row, col)
VP_LAYOUT <- function(x,y)
{
	viewport(layout.pos.row=x, layout.pos.col=y)
}

#############################################################################################3


FILLED_HISTOGRAMS_MAKER <- 
	function(
		Input_Data,				# data.frame 
		Variable_Name_that_will_Color_Bars	# characters	
	)
{
	Names_of_Variables	 	<- colnames(Input_Data)
	
		# Frowing out the variable that fills histograms	
	Other_Names_than_Variable_Name 	<- 	setdiff(
							Names_of_Variables,
							Variable_Name_that_will_Color_Bars
						)
	
		# Making histograms
		# Note: scale_fill_discrete changest the legend name
	return
	(	
		lapply(
			Other_Names_than_Variable_Name, 
			function(x)
			{ 
				qplot( 
					eval(parse(text = x)), 
					data = Input_Data, 
					geom="bar", 
					ylab="No of people", 
					xlab=gsub("_", " ", x),
					fill=eval(parse(text = Variable_Name_that_will_Color_Bars)),
					position="stack"
				) + coord_flip() + scale_fill_discrete(name=gsub("_", " ", Variable_Name_that_will_Color_Bars))
			}	
		)
	)
}	

#############################################################################################3


HIST_LIST_UNWRAPPED <- function( Input_Data )
{	
	Variable_Names 	<- colnames( Input_Data )
	No_of_Variables	<- length(Variable_Names)
	
	tmp 	<- vector("list", No_of_Variables)

	for (i in 1:No_of_Variables)
	{
		if 
		(
			length(
				levels(
					eval(
						parse(
							text = 	paste(
									"Data$",
									Variable_Names[i], 
									sep=""
								)
						)
					)
				)
			) == 0
		) 	
		{
			tmp[[i]] <- list("")
		}
		else
		{
			tmp[[i]] <- FILLED_HISTOGRAMS_MAKER(Data, Variable_Names[i])
		}
	}
	return(tmp)
}


#############################################################################################3

	#Function that helps us forget about the inner numbering of variables
	
N	<- function(x)
{
	if (is.element( x, Names_of_Variables_Polish )) 
	{ return( which( Names_of_Variables_Polish == x ) ) }
	else 
	
	if (is.element( x, Names_of_Variables_English )) return( which( Names_of_Variables_English == x ) )
	else return("error")
}



#############################################################################################3


GET_HISTOGRAM 	<- function(Binned_Variable, Filling_Variable)
{
	x <- N(Filling_Variable)
	y <- N(Binned_Variable)
	if (x < y ) return(Filled_Histograms[[x]][[y-1]])
	else
	return( Filled_Histograms[[x]][[y]] )
}



#############################################################################################3
	# The results of a model that are used by xtable are usually the 
	# Model$coefficients part anyway. We take it, and change the names of rows
	# so that they look funky.
	
TRANSLATE_RESULTS <- function( Model_Coefficients )
{
	
	rownames(Model_Coefficients) <-	
		as.vector(
			sapply( 
				rownames( Model_Coefficients ),
				function( Name_of_var_in_row_in_summary ) 
				{	
						# Je??li zmienna nie jest opisana za pomoc?? factor.
						# Czyli wyniki modelowania nie zwracaj?? factor values przy jej nazwie.
					if ( is.element(Name_of_var_in_row_in_summary, Names_of_Variables_English) )
					return ( gsub("_", " ", Name_of_var_in_row_in_summary) )
					else
					{
							# Okre??l kt??re wyra??enie wyst??puje jako nazwa zmiennej w wynikach.
						Prefix 	<- 	Names_of_Variables_English[
								sapply( 
									Names_of_Variables_English, 
									function(y) grepl(y, Name_of_var_in_row_in_summary ) 
								)]
		
						if (length(Prefix) == 0) return( Name_of_var_in_row_in_summary )		
						else
						return(
							paste( 
								gsub("_", " ", Prefix) ,
								DICTIONARY_POLISH_ENGLISH(sub( Prefix,"", Name_of_var_in_row_in_summary )), 
								sep=" : "
							)		
						)
					}			
				}
			)
		)
	return(
		Model_Coefficients
	)	
}


#############################################################################################3
	# Calculate the relative change of the conditional mean, which is not so much conditional.

RELATIVE_CHANGE <- function(x)
{
	return( exp(x) - 1 )
}

