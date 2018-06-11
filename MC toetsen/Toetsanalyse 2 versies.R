################################################################################
### Toetsanalyse 2 versies.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Toetsanalyse 2 versies.R
### Doel: Manipuleren en analyseren van teleform tentamendata voor 
### tentamen met 2 versies en 4 antwoordalternatieven
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
### 07-03-2018: DD: Aanmaken bestand
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
                   dplyr::filter(studentnummers > 0, Toetsversie < 3)

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
# volgorde <- read_xlsx(paste0(Network_directory,"Volgordeomzetting.xlsx")) %>% map_df(as.integer)
orderB <- as.vector(volgorde$Bversie)

##Extraheer studentnummers en namen (wordt later correct gedaan, kan hier weg)
# studentnummers_namen <- teleformdata %>%  
#   dplyr:: filter(stud_nr > 0, Toetsversie <=2) %>% 
#   dplyr:: select(stud_nr, stud_naam) 
# colnames(studentnummers_namen) <- c("studentnummers", "studentnamen")

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

## Sla studentnummers en namen op, voor latere koppeling aan gescoorde data
studentnummers_namen <- teleformdata_correct %>% 
  dplyr:: select(c(stud_nr, stud_naam))

##Verwijder vragen uit dataset (optioneel te gebruiken)
# data <- dplyr:: select(data, -V5, -V13, -V14, -V17, -V30, -V36)
# nrq <- 34
# nrc <- nrq+2

write.csv2(data, file=paste0(Network_directory,"data.csv"), row.names=FALSE)

##########################################################
## Upload eventueel aangepaste/nieuwe sleutel
## Om meerdere antwoorden goed te rekenen lever komma gescheiden aan
sleutel <- read.csv2(paste0(Network_directory,"sleutel.csv"))

## Vervang lege cellen met NA zodat deze goed gescoord worden
data[] <- lapply(data, str_trim)
is.na(data) <- data==''

##Transformeren van ruwe letter_data naar score data + basale analyse
scored_data <- score_mc(data, sleutel, multiKeySep = ",", 
                        output.scored = TRUE, rel = TRUE)

##Toevoegen studentnummers en namen aan score data
scored_datax <- cbind(studentnummers_namen, scored_data$scored)

##Toevoegen studentnummers aan totaalscore student
total_score <- cbind(studentnummers_namen, scored_data[1])
total_score <- total_score %>% rename(studentnummers = stud_nr)

##Transformeer scores naar cijfers
total_score <- mutate(total_score, cijfer = (10-(nrq-total_score$score)/(nrq-cesuur)*(10-5.5)))
total_score <-  total_score %>% mutate(cijfer = replace(cijfer, cijfer<1, 1))

##Wegschrijven score per student naar csv file
write.csv2(total_score, file=paste0(Network_directory,"results_student.csv"), 
           row.names=FALSE)

## Toon cronbachs alpha
KR20 <- purrr:: pluck(scored_data, 2, "alpha")
# KR20 <- scored_data$reliability$alpha

##Bereken KR-20 (75)
ifactor <- 75/nrq
KR20_75 <- round(CTT:: spearman.brown(KR20, input = ifactor, n.or.r = "n")$r.new, digits = 2)

##Item characteristic curves (ICC) voor alle items op 1 pagina 
##(verwijder eerste 2 regels script om losse plots te creeren)
par(mfrow=c(4,5)) 
par(cex = 0.4)
for ( i in 1:nrq ) cttICC(scored_data$score, scored_data$scored[,i], 
                          colTheme="spartans", cex=1.5, ylab=names(sleutel[i]))

##Maak itemanalyse
itemanalyse <- itemAnalysis(as.data.frame(scored_data$scored), NA.Delete=FALSE)$itemReport %>% 
  dplyr:: select(-bis) %>% 
  dplyr::rename(P_waarde = itemMean,
                rir = pBis,
                "New Alpha" = alphaIfDeleted)

##NA vervangen met nullen
itemanalyse[is.na(itemanalyse)] <- 0

##Voeg P' column toe aan itemanalyse
itemanalyse["Rel_P"] <- NA

##Bereken relatieve p-waarde
for ( i in 1:nrq ) itemanalyse$Rel_P[i] <- ((-1/(gk-1))*itemanalyse$P_waarde[i]+1-(-1/(gk-1)))

##Toetswaarden  wegschrijven
geslaagd <- filter(total_score, cijfer >= 5.5) %>% nrow()

toets <- tbl_df(scored_data$reliability[1:5]) %>% round(digits = 2)
toets <- mutate(toets, KR20_75 = KR20_75) %>% 
  dplyr:: select(nItem, 
                 nPerson, 
                 alpha,
                 KR20_75,
                 scaleMean,
                 scaleSD) %>% 
  dplyr:: mutate(meanRelP = round(summarise(itemanalyse, mean(Rel_P))$`mean(Rel_P)`, digits = 2),
                 meanP = round(summarise(itemanalyse, mean(P_waarde))$`mean(P_waarde)`, digits = 2),
                 perc_geslaagd = paste0(round(geslaagd/nrow(total_score)*100),"%"),
                 cesuur = cesuur)

##Berekenen kappa
kappa <- round(((KR20)*(toets$scaleSD^2)+(toets$scaleMean-cesuur)^2)/((toets$scaleSD^2) + (toets$scaleMean-cesuur)^2), digits = 2)
toets <- mutate(toets, kappa = as.numeric(kappa))
write.csv2(toets, file=paste0(Network_directory,"toetswaarden.csv"))

##Bepaal aantal studenten
nrst <- toets$nPerson

## Vervang NA in data door lege cel
data[is.na(data)] <- " "  

##Toevoegen A-waarde aan itemanalyse
itemanalyse["A"] <- NA
itemanalyse["B"] <- NA
itemanalyse["C"] <- NA

if (nra == 4) {
  itemanalyse["D"] <- NA 
}

if (nra == 5) {
  itemanalyse["E"] <- NA
}


for ( i in 1:nrq ) itemanalyse$A[i] <- (sum(str_count(data[,i], "A"))/nrst)
for ( i in 1:nrq ) itemanalyse$B[i] <- (sum(str_count(data[,i], "B"))/nrst)
for ( i in 1:nrq ) itemanalyse$C[i] <- (sum(str_count(data[,i], "C"))/nrst)

if (nra == 4) {
  for ( i in 1:nrq ) itemanalyse$D[i] <- (sum(str_count(data[,i], "D"))/nrst)
}


if (nra == 5) {
  for ( i in 1:nrq ) itemanalyse$E[i] <- (sum(str_count(data[,i], "E"))/nrst)
}


##Voeg advies column toe aan itemanalyse
itemanalyse[".A"] <- NA
itemanalyse[".B"] <- NA
itemanalyse[".C"] <- NA
itemanalyse[".D"] <- NA
itemanalyse[".E"] <- NA

##Genereer advies op basis van P- en rirwaarden
for ( i in 1:nrq ) if( (itemanalyse$Rel_P[i] + itemanalyse$rir[i] < 0.4) ){
  itemanalyse$.E[i] <- "E"
}

for ( i in 1:nrq ) if( (itemanalyse$P_waarde[i] < (gk+0.04))&(itemanalyse$rir[i] > 0.05) ){
  itemanalyse$.D[i] <- "D"
}

for ( i in 1:nrq ) if( (itemanalyse$P_waarde[i] < 0.3)&((itemanalyse$rir[i] <= 0.05)&(itemanalyse$rir[i] >= -0.05)) ){
  itemanalyse$.C[i] <- "C"
}

for ( i in 1:nrq ) if( (itemanalyse$Rel_P[i] < 0.4)&(itemanalyse$rir[i] <= 0.10) ){
  itemanalyse$.A[i] <- "A"
}

for ( i in 1:nrq ) if( (itemanalyse$Rel_P[i] < 0.8)&(itemanalyse$rir[i] < -0.10) ){
  itemanalyse$.B[i] <- "B"
}

##Verander kolom volgorde itemanalyse
if (nra == 3) {
  itemanalyse <- itemanalyse %>% dplyr::select(itemName, A, B, C, P_waarde, Rel_P, rir,
                                               `New Alpha`, .A, .B, .C, .D, .E)
}

if (nra == 4) {
  itemanalyse <- itemanalyse %>% dplyr::select(itemName, A, B, C, D, P_waarde, Rel_P, rir,
                                               `New Alpha`, .A, .B, .C, .D, .E)
}

if (nra == 5) {
  itemanalyse <- itemanalyse %>% dplyr::select(itemName, A, B, C, D, E, P_waarde, Rel_P, rir,
                                               `New Alpha`, .A, .B, .C, .D, .E)
}

##Verwijder NA's uit itemanalyse
itemanalyse[,10:14] <- sapply(itemanalyse[,10:14], as.character)
itemanalyse[,10:14][is.na(itemanalyse[,10:14])] <- " "

## Voeg gebruikte sleutel toe aan itemanalyse
tsleutel <- as.data.frame(t(sleutel))
itemanalyse <- cbind(tsleutel, itemanalyse) %>% 
  dplyr:: rename(Key = V1)

itemanalyse <- dplyr:: mutate(itemanalyse, itemName = colnames(sleutel))

##Schrijf itemanalyse weg naar csv
write.csv2(itemanalyse, row.names = F , file=paste0(Network_directory,
                                                    "itemanalyse.csv"))

##Bereken gemiddelde score en sd per toetsversie
versie_score <- inner_join(total_score, student_versies, by = "studentnummers") %>% group_by(Toetsversie) %>%
  summarise(mean=mean(score), sd=sd(score), n=n())

ttest <- tsum.test(mean.x=versie_score$mean[1],   s.x=versie_score$sd[1], n.x=versie_score$n[1],
                   mean.y=versie_score$mean[2], s.y=versie_score$sd[1], n.y=versie_score$n[2])

try(if(ttest$p.value < 0.05) stop("Gemiddelde score versies verschillen significant"))


