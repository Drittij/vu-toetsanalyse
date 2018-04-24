################################################################################
### Heranalyse Toetsanalyse 2 versies stap 2.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: eranalyse Toetsanalyse 2 versies stap 2.R
### Doel: Analyseren van teleform tentamendata voor 
### tentamen met 2 versies en 3, 4 of 5 antwoordalternatieven voor heranalyse
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


