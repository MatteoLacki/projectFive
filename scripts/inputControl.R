	# Deleting non-consistent or non-needed rows: general scheme. Add it before adding smoking group.

# Inconsistency 1	
	# structure of questions 44-46:
	# 44. Czy pali Pani (kwestionariusze_2011.pdf są w wersji dla kobiet. 
	# Spodziewam się, że dla mężczyzn nr pytania jest zachowany)
	# papierosy?
	# 45. jeżeli tak, to ile przeciętnie sztuk papierosów dziennie ... ?
	# 46. jeżeli nie, to czy kiedykolwiek w życiu paliła Pani papierosy? 


conditionEnumeratingInconsistentObservations <-
	( 
		is.na(Data$Smokes) & 
		( 
			(
				is.na(Data$Ever_Smoked) & 
				(
					(
						is.na(Data$Daily_Smokes) 
						# Non-informative rows OR ...
					) |	
					(
						!is.na(Data$Daily_Smokes) & Data$Daily_Smokes==0
							# ... people that smoke 0 cigarettes but at the same time can be assigned to both the group of 
							# people that never ever smoked and to former smokers as well. 
					)	
				)
			) | 
			( 
				!is.na(Data$Ever_Smoked) & !is.na(Data$Daily_Smokes) & Data$Daily_Smokes>0
					# if Daily_Smokes>0 then Smokes='yes' and so the pollee could not responded to question Ever_Smoked 
					# and so this should be set to NA, but it ain't.
			)
		) 
	) |
	(
		!is.na(Data$Smokes) &		# Smokes <> NA
		(
			( 
		 		Data$Smokes == 'no' & 
	 			( 
	 				(
	 					is.na(Data$Ever_Smoked) 
	 						# Again: can be assigned to people that never smoked and to former smokers as well.
	 				) |							
	 				( 
	 					!is.na(Data$Ever_Smoked) & !is.na(Data$Daily_Smokes) & Data$Daily_Smokes>0
	 						# if Smokes='no' then it cannot happen that Daily_Smokes>0.
	 				) 
	 			) 
		 	) |
	 		( 
	 			Data$Smokes == 'yes' & 
	 			(
	 				(
	 					!is.na(Data$Ever_Smoked)
	 						# cannot have Smokes='yes' and non-NA entry in Ever_Smoked: Inconsistency 1.
	 				) | 
	 				( 
	 					is.na(Data$Ever_Smoked) & 
	 					( 
	 						(
	 							is.na(Data$Daily_Smokes)
	 								# cannot have Smokes='yes' and non-NA entry in Ever_Smoked: Inconsistency 1. 
	 						) |
	 						(
	 							!is.na(Data$Daily_Smokes) & Data$Daily_Smokes == '0' 
	 								# smokes='yes', but either did not respond to Daily_Smoked (NA) 
	 								# or responded in a inconsistent way (0 cigarettes).
	 						)
	 					) 
	 				)
	 			) 
	 		)
		) 
	)

	# Elimination of inconsistent observations

Data <- Data[ !conditionEnumeratingInconsistentObservations ,]	
#summary(Data)

		# Conditions for making changes in cases, where it is needed
	
	# Changing values of Smokes_Daily to 0.
conditionChangingSmokesDailyToZero <-
	(
		!is.na(Data$Smokes) & 
		(
			Data$Smokes == 'no' & !is.na(Data$Ever_Smoked) & is.na(Data$Daily_Smokes)
				# All observations where Smokes='no', Ever_Smoked answered properly (not NA), 
				# and no info on how much people smoke daily (Daily_Smokes = NA). 
				# Note that if there was already Daily_Smokes=0, then we leave it there.
		)
	) |
	(
		is.na(Data$Smokes) & 
		(
			!is.na(Data$Ever_Smoked) & is.na(Data$Daily_Smokes)
				# We know that Ever_Smoked was properly answered. 
				# Thus a person cannot smoke: Inconsistency 1.
				# So we can set Daily_Smokes to 0.
		)
	)

		# Actual Change --> check it!
Data[conditionChangingSmokesDailyToZero,'Daily_Smokes'] <- 0
#summary(Data)

	# Changing values of Smokes to 'yes'. 
conditionChangingSmokesToYes <-
	is.na(Data$Smokes) & is.na(Data$Ever_Smoked) & !is.na(Data$Daily_Smokes) & Data$Daily_Smokes>0
		# Observation: these people will have Smoke='yes' and Daily_Smokes>0.

		# Actual Change
Data[conditionChangingSmokesToYes,'Smokes'] <- 'yes'	
#summary(Data)

	# Changing values of Smokes to 'no'.

conditionChangingSmokesToNo <-
	is.na(Data$Smokes) & 
	(
		!is.na(Data$Ever_Smoked) & 
		(
			(
				is.na(Data$Daily_Smokes)
			) | 
			(	
				Data$Daily_Smokes==0
			)
			# Smokes is unknown. But a response was given for ever smoked. 
			# If the Daily_Smokes has now the correct value (NA or 0),
			# then we say Smokes='no'.
		)
	)

		# Actual Change
Data[conditionChangingSmokesToNo,'Smokes'] <- 'no'
#summary(Data)

	# Changing values of Ever_Smoked to 'no'. Not needed.

#conditionChangingEverSmokedToNo <-
#	!is.na(Data$Smokes) & Data$Smokes=='yes' & is.na(Data$Ever_Smoked) & !is.na(Data$Daily_Smokes) & Data$Daily_Smokes>0
		# We impute from the correct values Smoke='yes', Daily_Smokes>0 that Ever_Smoked='no' : Inconsistency 1.

		# Actual Change
#Data[conditionChangingEverSmokedToNo,'Ever_Smoked'] <- 'no'
#summary(Data) # Extremely wrong.

rm(conditionEnumeratingInconsistentObservations, conditionChangingSmokesDailyToZero, conditionChangingSmokesToYes,conditionChangingSmokesToNo )
