# Hello and welcome to project 5. We're going to speak about tobacco and get some lolly out of the poppy!

load("data/diagnozaOsoby2011.RData")

  # let us settle for lower camel convention in naming variables and functions: 
  # http://stackoverflow.com/questions/1944910/what-is-your-preferred-style-for-naming-variables-in-r

  # Dodać płeć, wiek, dochód, grupę zawodową.  

variablesOriginalNames  <- c("ap83.1", "ap83.2", "ap83.3", "ap84", "ap85", "ap86", "ap100", "ac8")

   
variablesDescriptionPolish  <- c( "Czy pali papierosy", 
                              "Ile przecietne papierosow dziennie wypala", 
                              "Czy kiedykolwiek palil papierosy",
                              "Korzystalem z porad psychologa (psychiatry)",
                              "Pilem za duzo alkoholu",
                              "Probowalem narkotykow",    
                              "Oskarzono mnie o dokonanie czynu karalnego",
                              "Stan cywilny"     
                              )
                              
variablesDescriptionEnglish <- c( "smokes", 
                                  "daily smoked cigarettes",  # Check >= 0 if not NA.
                                  "ever smoked",              # Maybe frow out obs where smokes=yes and ever_smoked = no. 
                                  "psychic treatment",
                                  "former alcohol addict",
                                  "tried drugs",    
                                  "accused of offence",   
                                  "marital status"            #translation: http://en.wikipedia.org/wiki/Marital_status
                                )

# Interesting questions: (add more please - we will make a vote what to choose)

  #  Is there an effect of mass exposure to addicting things: how big is the percentage of people that smoke, tried drugs, were alcohol addicts.
  #  Maybe add cancer - as to measure exposure?? 
      # 594;"bp110.7";"Szansa: zachorowanie na raka"  -- dunno what is it really... but looks promissing.
  
