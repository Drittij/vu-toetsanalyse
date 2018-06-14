################################################################################
### Heranalyse Toetsanalyse 4 versies stap 1.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Heranalyse Toetsanalyse 4 versies stap 1.R
### Doel: Stap 1 in het manipuleren van teleform tentamendata voor 
### tentamen met 4 versies
### 
### Afhankelijkheden: geen
###
### Gebruikte datasets: Teleform .DEL bestand
###
### Opmerkingen: Dit script werkt alleen goed met tentamens met 4 versies
### 
################################################################################
### TODO:
### 1) Testen
###
################################################################################    
### Geschiedenis:
### 14-06-2018: DD: Aanmaken bestand
################################################################################

##defineer aantal columns
nrc <- nrq+2

##Bepaal gokkans
gk <- 1/nra

#Maak 2 datasets op basis van versie en verwijder studenten zonder versie
teleformdataA <- teleformdata %>% dplyr:: filter(Toetsversie == 1)
teleformdataB <- teleformdata %>% dplyr:: filter(Toetsversie == 2)
teleformdataC <- teleformdata %>% dplyr:: filter(Toetsversie == 3)
teleformdataD <- teleformdata %>% dplyr:: filter(Toetsversie == 4)
teleformdata_onbekend <- teleformdata %>% dplyr:: filter(Toetsversie >4)

##Maak bestand met studentnummer + Toetsversie voor latere koppeling aan score
student_versies <- dplyr:: select(teleformdata, studentnummers=stud_nr, 
                                  Toetsversie) %>% 
  dplyr::filter(studentnummers > 0)

##Maak ruwe data file: letter data + sleutel
teleformdata_new <- teleformdata[ c(1:nrc) ]
teleformdataA_new <- teleformdataA[ c(1:nrc) ]
teleformdataB_new <- teleformdataB[ c(1:nrc) ]
teleformdataC_new <- teleformdataC[ c(1:nrc) ]
teleformdataD_new <- teleformdataD[ c(1:nrc) ]

##Defineer vraagnamen aanwezige vragen
vrn <- colnames(teleformdata_new[3:nrc])

##Extraheer sleutel
sleutel <- teleformdataA_new %>% dplyr:: filter(stud_nr == 0) %>% 
  dplyr:: select(-c(stud_nr, stud_naam))

write.csv2(sleutel, file=paste0(Network_directory,"sleutel.csv"), 
           row.names=FALSE)

##Bepaal nieuwe volgorde vragen B, C naar A versie
volgorde <- read.csv2(paste0(Network_directory,"Volgordeomzetting.csv"))
# volgorde <- read_xlsx(paste0(Network_directory,"Volgordeomzetting.xlsx")) %>% map_df(as.integer)
orderB <- as.vector(volgorde$Bversie)
orderC <- as.vector(volgorde$Cversie)
orderD <- as.vector(volgorde$Dversie)

##Extraheer studentnummers en namen (wordt later gedaan, niet meer nodig)
# studentnummers_namen <- teleformdata %>%  
#   dplyr:: filter(stud_nr > 0, Toetsversie <=2) %>% 
#   dplyr:: select(stud_nr, stud_naam) 
# colnames(studentnummers_namen) <- c("studentnummers", "studentnamen")

##Verwijder eerste twee kolommen (=studentnamen en studentnummers)
teleformdataD_new <- teleformdataD_new %>% dplyr:: select(-c(stud_nr, stud_naam))
teleformdataC_new <- teleformdataC_new %>% dplyr:: select(-c(stud_nr, stud_naam))
teleformdataB_new <- teleformdataB_new %>% dplyr:: select(-c(stud_nr, stud_naam))
teleformdataA_new <- teleformdataA_new %>% dplyr:: select(-c(stud_nr, stud_naam))

##Zet data B en C versie in volgorde Aversie en verander kolomnamen zodat deze 
##overeen komen met A versie
teleformdataB_correct <- teleformdataB_new[,orderB]
teleformdataC_correct <- teleformdataC_new[,orderC]
teleformdataD_correct <- teleformdataD_new[,orderD]
names = c(colnames(sleutel[1:nrq]))
colnames(teleformdataB_correct) = names
colnames(teleformdataC_correct) = names
colnames(teleformdataD_correct) = names

##Toevoegen studentnummers aan juiste volgorde D versies
teleformdataD_correct <- cbind(teleformdataD$stud_nr, 
                               teleformdataD$stud_naam, teleformdataD_correct) %>% 
  dplyr:: rename(stud_nr = 'teleformdataD$stud_nr',
                 stud_naam = 'teleformdataD$stud_naam')

##Toevoegen studentnummers aan juiste volgorde c versies
teleformdataC_correct <- cbind(teleformdataC$stud_nr, 
                               teleformdataC$stud_naam, teleformdataC_correct) %>% 
  dplyr:: rename(stud_nr = 'teleformdataC$stud_nr',
                 stud_naam = 'teleformdataC$stud_naam')

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

##Voeg data versie A, B en C samen
teleformdata_correct <- rbind(teleformdataA, teleformdataB_correct, teleformdataC_correct, teleformdataD_correct)

###Extraheer data en verwijder eerste twee kolommen 
## (=studentnamen en studentnummers)
data <- teleformdata_correct %>% 
  dplyr:: select(-c(stud_nr, stud_naam))
