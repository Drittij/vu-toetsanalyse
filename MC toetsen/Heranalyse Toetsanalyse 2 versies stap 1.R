################################################################################
### Heranalyse Toetsanalyse 2 versies stap 1.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Heranalyse Toetsanalyse 2 versies stap 1.R
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

#Maak 2 datasets op basis van versie en verwijder studenten zonder versie
teleformdataA <- teleformdata %>% dplyr:: filter(Toetsversie == 1)
teleformdataB <- teleformdata %>% dplyr:: filter(Toetsversie == 2)
teleformdata_onbekend <- teleformdata %>% dplyr:: filter(Toetsversie >2)

##Maak bestand met studentnummer + Toetsversie voor latere koppeling aan score
student_versies <- dplyr:: select(teleformdata, studentnummers=stud_nr, 
                                  Toetsversie) %>% 
  dplyr::filter(studentnummers > 0)

##Maak ruwe data file: letter data + sleutel
teleformdata_new <- teleformdata[ c(1:nrc) ]
teleformdataA_new <- teleformdataA[ c(1:nrc) ]
teleformdataB_new <- teleformdataB[ c(1:nrc) ]

##Defineer vraagnamen aanwezige vragen
vrn <- colnames(teleformdata_new[3:nrc])

##Extraheer sleutel
sleutel <- teleformdataA_new %>% dplyr:: filter(stud_nr == 0) %>% 
  dplyr:: select(-c(stud_nr, stud_naam))

write.csv2(sleutel, file=paste0(Network_directory,"sleutel.csv"), 
           row.names=FALSE)

##Bepaal nieuwe volgorde vragen B naar A versie
volgorde <- read.csv2(paste0(Network_directory,"Volgordeomzetting.csv"))
orderB <- as.vector(volgorde$Bversie)

##Extraheer studentnummers en namen
studentnummers_namen <- teleformdata %>%  
  dplyr:: filter(stud_nr > 0, Toetsversie <=2) %>% 
  dplyr:: select(stud_nr, stud_naam) 
colnames(studentnummers_namen) <- c("studentnummers", "studentnamen")

##Verwijder eerste twee kolommen (=studentnamen en studentnummers)
teleformdataB_new <- teleformdataB_new %>% dplyr:: select(-c(stud_nr, stud_naam))
teleformdataA_new <- teleformdataA_new %>% dplyr:: select(-c(stud_nr, stud_naam))

##Zet data B versie in volgorde Aversie en verander kolomnamen zodat deze 
##overeen komen met A versie
teleformdataB_correct <- teleformdataB_new[,orderB]
names = c(colnames(sleutel[1:nrq]))
colnames(teleformdataB_correct) = names

##Toevoegen studentnummers aan juiste volgorde b versies
teleformdataB_correct <- cbind(teleformdataB$stud_nr, 
                               teleformdataB$stud_naam, teleformdataB_correct) %>% 
  dplyr:: rename(stud_nr = 'teleformdataB$stud_nr',
                 stud_naam = 'teleformdataB$stud_naam')

##Toevoegen studentnummers aan a versie
teleformdataA <- cbind(teleformdataA$stud_nr, 
                       teleformdataA$stud_naam, teleformdataA_new) %>% 
  dplyr:: rename(stud_nr = 'teleformdataA$stud_nr',
                 stud_naam = 'teleformdataA$stud_naam') %>% 
  dplyr:: filter(stud_nr > 0) 

##Voeg data versie A en B samen
teleformdata_correct <- rbind(teleformdataA, teleformdataB_correct)

###Extraheer data en verwijder eerste twee kolommen 
## (=studentnamen en studentnummers)
data <- teleformdata_correct %>% 
  dplyr:: select(-c(stud_nr, stud_naam))
