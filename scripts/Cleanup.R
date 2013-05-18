load("data/diagnozaOsoby2011.RData")

variablesOriginalNames  <- c("ap83_1", "ap83_2", "ap83_3", "ap84", "ap85", "ap86", "ap100", "ac8",
                             "wiek2011", "wiek6_2011", "status9_2011", "eduk4_2011", "PLEC", "bp107")


variablesDescriptionPolish  <- c( "Czy pali papierosy", 
                                  "Ile przecietnie papierosow dziennie wypala", 
                                  "Czy kiedykolwiek palil papierosy",
                                  "Korzystalem z porad psychologa (psychiatry)",
                                  "Pilem za duzo alkoholu",
                                  "Probowalem narkotykow",    
                                  "Oskarzono mnie o dokonanie czynu karalnego",
                                  "Stan cywilny",
                                  "Wiek",
                                  "Kategoria wiekowa", 
                                  "Grupa zawodowa", 
                                  "Poziom wyksztalcenia",
                                  "Plec",
                                  "Dochod miesieczny",
                                  "Kategorie palaczy")

variablesDescriptionEnglish <- c( "smokes", 
                                  "daily smoked cigarettes",  # Check >= 0 if not NA.
                                  "ever smoked",              # Maybe frow out obs where smokes=yes and ever_smoked = no. 
                                  "psychiatric treatment",
                                  "former alcohol addict",
                                  "tried drugs",    
                                  "accused of offence",   
                                  "marital status",           #translation: http://en.wikipedia.org/wiki/Marital_status
                                  "age",
                                  "age group",
                                  "employment status",
                                  "education",
                                  "gender",
                                  "monthly income",
                                  "smoker group"
)

variablesNames <- c("Smokes",
                    "Daily_Smokes",
                    "Ever_Smoked",
                    "Psychiatric",
                    "Alcohol",
                    "Drugs",
                    "Criminal",
                    "Marital_Status",
                    "Age",
                    "Age_Group",
                    "Employment",
                    "Education",
                    "Gender",
                    "Income",
                    "Smoker_Group")

Data <- diagnozaOsoby2011[,variablesOriginalNames]
colnames(Data) <- variablesNames[1:14]

temp <- data.frame(ifelse(is.na(Data[c(1:7,11,12,14)]), T, NA))
temp[,'indeks'] = 1:nrow(Data)
Data <- Data[-na.omit(temp)[,'indeks'],]
rm(temp)

fixLevels <- function(lvls, d=NULL, var=NULL, order=NULL, skip=0) {
  f <- function(d1, var1) {
    if (skip > 0) {
      levels(d1[,var1]) <- c(levels(d1[,var1])[1:skip],lvls)
    } else {
      levels(d1[,var1]) <- lvls
    }  
    o <- T
    if (is.null(order)) { 
      o <- F
      order <- 1:length(lvls) 
    } else if (order == F) { o <- F }
    d1[,var1] <- factor(d1[,var1], 
                        levels = sapply(order, function(x) { lvls[x] }), 
                        ordered = o)
    d1
  }
  if (is.null(d)) { 
    f 
  } else {
    if (is.null(var)) { var <- colnames(d) }
    if (length(var) > 1) {
      for (x in var) {
        d <- f(d,x)
      }
    } else {
      d <- f(d,var)
    }
    d
  }
}

Data <- fixLevels(c("yes","no"), Data, c("Smokes", "Ever_Smoked","Psychiatric", "Drugs", "Criminal", "Alcohol"),skip=1)
Data$Daily_Smokes[is.na(Data$Daily_Smokes)] <- 0
Data <- fixLevels(c("single","married","widowed","divorced","separated","unknown"), Data, "Marital_Status",skip=2)
Data <- fixLevels(c("0-24","25-34","35-44","45-59","60-64","65+"),Data, "Age_Group",skip=1, order=T)
Data <- fixLevels(c("civil servant", "private sector", "entrepreneur", 
                    "farmer", "pensioner","retiree", "pupil or student", 
                    "unemployed", "other non-active"), Data, "Employment", skip=1)
Data <- fixLevels(c("primary or less", "technical", "secondary", "beyond secondary"), Data, "Education", skip=1, order=T)
Data <- fixLevels(c("male","female"),Data,"Gender", skip=1)

smokerLevels <- c('never smoked', 'former smoker', 'up to half a pack', 'up to one pack', 'more than one pack')

Data[,"Smoker_Group"] <- factor(ifelse(Data$Smokes == "no" & Data$Ever_Smoked == "no", smokerLevels[1],
                                 ifelse(Data$Smokes == "no" & Data$Ever_Smoked == "yes", smokerLevels[2],
                                  ifelse(Data$Smokes == "yes" & Data$Daily_Smokes <= 10, smokerLevels[3],
                                   ifelse(Data$Smokes == "yes" & Data$Daily_Smokes > 10  &Data$Daily_Smokes <= 20, smokerLevels[4],
                                          smokerLevels[5])))), 
                                levels = smokerLevels, ordered = T)
                             
attach(Data)

# save(Data, file="data/Data.RData")