load("./Data/diagnozaOsoby2011.RData")

# Prepariamo nomi delle colonne di interesse.
variables_of_interest <- c("WOJEWODZTWO", "KLASA_MIEJSOWOSCI", "PLEC","wiek2007", "dp3", "dc18", "dp72",  "dp56", "dp114", "dp115" )

  # This will create a new data set that will be of use. Thanks to that we will not need to download at
  # every compilation the diagnozaOsoby2011.RData. 
write.csv2(diagnozaOsoby2011[,variables_of_interest], "./Data/prepared_data.csv", row.names = FALSE)

# Dimentichiamo di dati non utilizzati. Così il computer fonzionera meglio.
# Lo fai sempre in tutti progetti. Altrimenti anche i piu pottenti computer saranno molto tristi.
rm(diagnozaOsoby2011) 