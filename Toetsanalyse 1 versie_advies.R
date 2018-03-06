################################################################
####Dit script werkt alleen goed met tentamens met 1 versie####
###############################################################

##defineer aantal columns
nrc <- nrq+2

##Bepaal gokkans
gk <- 1/nra

##Maak ruwe data file: letter data + sleutel
teleformdata_new <- teleformdata[ c(1:nrc) ]

# ##Verwijder vragen uit dataset (optioneel te gebruiken)
# teleformdata_new <- select(teleformdata_new, -V42, -V52)
# nrq <- 50
# nrc <- nrq+2

##Defineer vraagnamen aanwezige vragen
vrn <- colnames(teleformdata_new[3:nrc])

##Extraheer sleutel
sleutel <- teleformdata_new[1,3:nrc]
write.csv2(sleutel, file=paste0(Network_directory,"sleutel.csv"), row.names=FALSE)

##Verwijder eerste rij (=sleutel) uit data
teleformdata_new <- teleformdata_new[-1, , drop=F]

##Extraheer studentnummers
studentnummers <- as.data.frame(teleformdata_new$stud_nr)
colnames(studentnummers) <- "studentnummers"
  
####Extraheer data en verwijder eerste twee kolommen (=studentnamen en studentnummers)
data <- dplyr:: select(teleformdata_new, -stud_nr, -stud_naam)
write.csv2(data, file=paste0(Network_directory,"data.csv"), row.names=FALSE)

##########################################################
## Upload eventueel aangepaste/nieuwe sleutel
## Om meerdere antwoorden goed te rekenen lever komma gescheiden aan
sleutel <- read.csv2(paste0(Network_directory,"sleutel.csv"))

##Transformeren van ruwe letter_data naar score data + basale analyse
scored_data <- score_daniel(data, sleutel, multiKeySep = ",", output.scored = TRUE, rel = TRUE)

##Toevoegen studentnummers aan score data - datax
scored_datax <- cbind(studentnummers, scored_data$scored)

##Toevoegen studentnummers aan totaalscore student
total_score <- cbind(studentnummers, scored_data$score)

##Verander column naam van totaalscore per student
names(total_score)[names(total_score) == "scored_data$score"] <- "totaal_score"

##Transformeer scores naar cijfers
total_score <- mutate(total_score, cijfer = (10-(nrq-total_score$totaal_score)/(nrq-cesuur)*(10-5.5)))

for ( i in 1:nrq ) if(total_score$cijfer[i] < 1){
  total_score$cijfer[i] <- 1
}

##Extraheer studentnamen en schrijf weg naar csv
studentnamen <- as.data.frame(teleformdata_new$stud_naam)
colnames(studentnamen) <- "studentnamen"

##Toevoegen studentnamen aan totaalscore student
total_score <- cbind(studentnamen, total_score)

##Wegschrijven score per student naar csv file
write.csv2(total_score, file=paste0(Network_directory,"results_student.csv"), row.names=FALSE)

## Toon cronbachs alpha
KR20 <- scored_data$reliability$alpha

##Bereken KR-20 (75)
ifactor <- 75/nrq
KR20_75 <- spearman.brown(KR20, input = ifactor, n.or.r = "n")
KR20_75 <- KR20_75$r.new

##Item characteristic curves (ICC) voor alle items op 1 pagina (verwijder eerste 2 regels script om losse plots te creeren)
par(mfrow=c(4,5)) 
par(cex = 0.4)
for ( i in 1:nrq ) cttICC(scored_data$score, scored_data$scored[,i], colTheme="spartans", cex=1.5, ylab=names(sleutel[i]))

##Maak itemanalyse
pwaarde <- as.data.frame(scored_data$reliability$itemMean)
rir <- as.data.frame(scored_data$reliability$pBis)
newalpha <- as.data.frame(scored_data$reliability$alphaIfDeleted)

itemanalyse <- cbind(pwaarde, rir, newalpha)
names(itemanalyse)[names(itemanalyse) == "scored_data$reliability$itemMean"] <- "P_waarde"
names(itemanalyse)[names(itemanalyse) == "scored_data$reliability$pBis"] <- "rir"
names(itemanalyse)[names(itemanalyse) == "scored_data$reliability$alphaIfDeleted"] <- "New Alpha"

##NA vervangen met nullen
itemanalyse[is.na(itemanalyse)] <- 0

##Voeg P' column toe aan itemanalyse
itemanalyse["Rel_P"] <- NA

##Bereken relatieve p-waarde
for ( i in 1:nrq ) itemanalyse$Rel_P[i] <- ((-1/(gk-1))*itemanalyse$P_waarde[i]+1-(-1/(gk-1)))

##Toetswaarden  wegschrijven
toets <- as.data.frame(scored_data$reliability[1:5])
toets <- tbl_df(toets)
toets <- mutate(toets, KR20_75 = KR20_75)
toets <- toets[,c(1,2,3,6,4,5)]
mrelp <- summarise(itemanalyse, mean(Rel_P))
mp <- summarise(itemanalyse, mean(P_waarde))
toets <- mutate(toets, meanRelP = as.numeric(mrelp), meanP = as.numeric(mp))
geslaagd <- filter(total_score, cijfer >= 5.5) %>% nrow()
pgeslaagd <- round(geslaagd/nrow(total_score)*100)
toets <- mutate(toets, perc_geslaagd = pgeslaagd)
toets <- mutate(toets, cesuur = as.numeric(cesuur))

##Berekenen kappa
kappa <- ((KR20)*(toets$scaleSD^2)+(toets$scaleMean-cesuur)^2)/((toets$scaleSD^2) + (toets$scaleMean-cesuur)^2)
toets <- mutate(toets, kappa = as.numeric(kappa))
write.csv2(toets, file=paste0(Network_directory,"toetswaarden.csv"))

##Bepaal aantal studenten
nrst <- toets$nPerson

##Toevoegen A-waarde aan itemanalyse
itemanalyse["A"] <- NA
itemanalyse["B"] <- NA
itemanalyse["C"] <- NA
itemanalyse["D"] <- NA
# itemanalyse["E"] <- NA

library(stringr)
for ( i in 1:nrq ) itemanalyse$A[i] <- (sum(str_count(data[,i], "A"))/nrst)
for ( i in 1:nrq ) itemanalyse$B[i] <- (sum(str_count(data[,i], "B"))/nrst)
for ( i in 1:nrq ) itemanalyse$C[i] <- (sum(str_count(data[,i], "C"))/nrst)
for ( i in 1:nrq ) itemanalyse$D[i] <- (sum(str_count(data[,i], "D"))/nrst)
# for ( i in 1:nrq ) itemanalyse$E[i] <- (sum(str_count(data[,i], "E"))/nrst)

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
itemanalyse <- itemanalyse[,c(5,6,7,8,1,4,2,3,9,10,11,12,13)]

##Verwijder NA's uit itemanalyse
itemanalyse[,9:13] <- sapply(itemanalyse[,9:13], as.character)
itemanalyse[,9:13][is.na(itemanalyse[,9:13])] <- " "

## Voeg gebruikte sleutel toe aan itemanalyse
tsleutel <- as.data.frame(t(sleutel))
itemanalyse <- cbind(tsleutel, itemanalyse)
itemanalyse <- rename(itemanalyse, Key = V1)

##Schrijf itemanalyse weg naar csv
write.csv2(itemanalyse, row.names = vrn , file=paste0(Network_directory,"itemanalyse.csv"))

#Toon kappa
kappa

