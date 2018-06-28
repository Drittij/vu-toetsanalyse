################################################################################
### Heranalyse Toetsanalyse 1 versie stap 1.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Heranalyse Toetsanalyse 1 versie stap 1.R
### Doel: Stap 1 in het manipuleren van teleform tentamendata voor 
### tentamen met 2 versies en 3, 4 of 5 antwoordalternatieven
### 
### Afhankelijkheden: geen
###
### Gebruikte datasets: Teleform .DEL bestand
###
### Opmerkingen: Dit script werkt alleen goed met tentamens met 2 versies
### 
################################################################################
### TODO:
### 1) Geschikt maken voor meerdere versies
###
################################################################################    
### Geschiedenis:
### 24-04-2018: DD: Aanmaken bestand
################################################################################

##defineer aantal columns
nrc <- nrq+2

##Bepaal gokkans
gk <- 1/nra

##Maak ruwe data file zonder lege kolommen: letter data + sleutel
teleformdata_new <- teleformdata[ c(1:nrc) ]

# ##Verwijder vragen uit dataset (optioneel te gebruiken)
# teleformdata_new[delete] <- list(NULL)
# nrq <- nrq_nieuw
# nrc <- nrq+2

##Defineer vraagnamen aanwezige vragen
vrn <- colnames(teleformdata_new[3:nrc])

##Extraheer sleutel
sleutel <- teleformdata_new %>% dplyr:: filter(stud_nr == 0) %>% 
  dplyr:: select(-c(stud_nr, stud_naam))

write.csv2(sleutel, file=paste0(Network_directory,"sleutel.csv"), 
           row.names=FALSE)

##Extraheer studentnummers en namen
studentnummers <- teleformdata_new %>%  dplyr:: select(stud_nr) %>% 
  dplyr:: filter(stud_nr > 0) %>% 
  dplyr:: rename(studentnummers = stud_nr)


studentnamen <- teleformdata_new %>%  dplyr:: filter(stud_nr > 0) %>% 
  dplyr:: select(stud_naam) %>% 
  dplyr:: rename(studentnamen = stud_naam)

###Extraheer data en verwijder eerste twee kolommen 
## (=studentnamen en studentnummers)
data <- teleformdata_new %>% dplyr:: filter(stud_nr > 0) %>% 
  dplyr:: select(-c(stud_nr, stud_naam))

write.csv2(data, file=paste0(Network_directory,"data_origineel.csv"), row.names=FALSE)