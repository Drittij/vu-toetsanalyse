################################################################################
### Heranalyse Toetsanalyse meerdere versies stap 2.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Heranalyse Toetsanalyse meerdere versies stap 2.R
### Doel: Analyseren van teleform tentamendata voor 
### tentamen met meerdere versies
###
### Afhankelijkheden: geen
###
### Gebruikte datasets: Teleform .DEL bestand
###
### Opmerkingen: 
### 
################################################################################
### TODO:
### 1) Testen
###
################################################################################    
### Geschiedenis:
### 09-08-2018: DD: Aanmaken bestand
################################################################################

## Vervang lege cellen met NA zodat deze goed gescoord worden
data[] <- lapply(data, str_trim)
is.na(data) <- data==''

##Transformeren van ruwe letter_data naar score data + basale analyse
scored_data <- score_mc(data, sleutel, multiKeySep = ",", 
                        output.scored = TRUE, rel = TRUE)

studentnummers_namen <- teleformdata_correct[1:2]

##Toevoegen studentnummers en namen aan score data
scored_datax <- cbind(studentnummers_namen, scored_data$scored)

##Toevoegen studentnummers aan totaalscore student
total_score <- cbind(studentnummers_namen, scored_data[1])

##Transformeer scores naar cijfers
total_score <- mutate(total_score, cijfer = (10-(nrq-total_score$score)/(nrq-cesuur)*(10-5.5)))
total_score <-  total_score %>% mutate(cijfer = replace(cijfer, cijfer<1, 1))

total_score <- dplyr:: rename(total_score, studentnamen = stud_naam, studentnummers = stud_nr) %>% 
  mutate(studentnummers = as.integer(studentnummers))

## Toon cronbachs alpha
KR20 <- purrr:: pluck(scored_data, 2, "alpha")
# KR20 <- scored_data$reliability$alpha

##Bereken KR-20 (75)
ifactor <- 75/nrq
KR20_75 <- round(CTT:: spearman.brown(KR20, input = ifactor, n.or.r = "n")$r.new, digits = 2)

##Item characteristic curves (ICC) voor alle items op 1 pagina 
##(verwijder eerste 2 regels script om losse plots te creeren)
# par(mfrow=c(4,5)) 
# par(cex = 0.4)
# for ( i in 1:nrq ) cttICC(scored_data$score, scored_data$scored[,i], 
#                           colTheme="spartans", cex=1.5, ylab=names(sleutel[i]))

##Maak itemanalyse
itemanalyse <- itemAnalysis(as.data.frame(scored_data$scored), NA.Delete=FALSE)$itemReport %>% 
  dplyr:: select(-bis) %>% 
  dplyr::rename(P_waarde = itemMean,
                rir = pBis,
                "New Alpha" = alphaIfDeleted)

##NA vervangen met nullen
itemanalyse[is.na(itemanalyse)] <- 0

##Bereken relatieve p-waarde
itemanalyse <- itemanalyse %>% 
  mutate(Rel_P = ((-1/(gk-1))*P_waarde+1-(-1/(gk-1))))

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

##Bepaal aantal studenten
nrst <- toets$nPerson

## Vervang NA in data door lege cel
data[is.na(data)] <- " "  

##Toevoegen A-waarde aan itemanalyse
itemanalyse["A"] <- NA
itemanalyse["B"] <- NA

if (nra >= 3) {
  itemanalyse["C"] <- NA 
}

if (nra >= 4 ) {
  itemanalyse["D"] <- NA 
}

if (nra >= 5) {
  itemanalyse["E"] <- NA
}

if (nra >= 6) {
  itemanalyse["F"] <- NA
}


for ( i in 1:nrq ) itemanalyse$A[i] <- (sum(str_count(data[,i], "A"))/nrst)
for ( i in 1:nrq ) itemanalyse$B[i] <- (sum(str_count(data[,i], "B"))/nrst)

if (nra >= 3) {
  for ( i in 1:nrq ) itemanalyse$C[i] <- (sum(str_count(data[,i], "C"))/nrst)
}

if (nra >= 4) {
  for ( i in 1:nrq ) itemanalyse$D[i] <- (sum(str_count(data[,i], "D"))/nrst)
}

if (nra >= 5) {
  for ( i in 1:nrq ) itemanalyse$E[i] <- (sum(str_count(data[,i], "E"))/nrst)
}

if (nra >= 6) {
  for ( i in 1:nrq ) itemanalyse$'F'[i] <- (sum(str_count(data[,i], "F"))/nrst)
}

##Genereer advies op basis van P- en rirwaarden
itemanalyse <- itemanalyse %>% 
  mutate(.A = if_else(Rel_P < 0.4 & rir <= 0.10, "A", ""), 
         .B = if_else(Rel_P < 0.7 & rir < -0.10, "B", ""),
         .C = if_else(P_waarde < 0.3 & rir <= 0.05 & rir >= -0.05, "C", ""),
         .D = if_else(P_waarde < (gk+0.04) & rir > 0.05, "D", ""),
         .E = if_else(Rel_P + rir < 0.4, "E", ""))

##Verander kolom volgorde itemanalyse
if (nra == 2) {
  itemanalyse <- itemanalyse %>% dplyr::select(itemName, A, B, P_waarde, Rel_P, rir,
                                               `New Alpha`, .A, .B, .C, .D, .E)
}

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

if (nra == 6) {
  itemanalyse <- itemanalyse %>% dplyr::select(itemName, A, B, C, D, E, 'F', P_waarde, Rel_P, rir,
                                               `New Alpha`, .A, .B, .C, .D, .E)
}


## Voeg gebruikte sleutel toe aan itemanalyse
tsleutel <- as.data.frame(t(sleutel))
itemanalyse <- cbind(tsleutel, itemanalyse) %>% 
  dplyr:: rename(Key = V1)

itemanalyse <- dplyr:: mutate(itemanalyse, itemName = colnames(sleutel))
itemanalyse <- dplyr:: rename(itemanalyse, Item = itemName, P = P_waarde, 'P\''= Rel_P )


##Bereken gemiddelde score en sd per toetsversie
versie_score <- inner_join(total_score, student_versies, by = "studentnummers") %>% group_by(Toetsversie) %>%
  summarise(mean=mean(score), sd=sd(score), n=n())

ttest <- tsum.test(mean.x=versie_score$mean[1],   s.x=versie_score$sd[1], n.x=versie_score$n[1],
                   mean.y=versie_score$mean[2], s.y=versie_score$sd[2], n.y=versie_score$n[2])

if(ttest$p.value < 0.05) {
  write.csv2(versie_score, paste0(Network_directory,"Versie_score_verschillen.csv"))
  
  print("Gemiddelde score versie B en A verschillen significant")
  profvis::pause(20)
  
}

if (nrv == 3) {

ttest2 <- tsum.test(mean.x=versie_score$mean[1],   s.x=versie_score$sd[1], n.x=versie_score$n[1],
                   mean.y=versie_score$mean[3], s.y=versie_score$sd[3], n.y=versie_score$n[3])

if(ttest2$p.value < 0.05) {
  write.csv2(versie_score, paste0(Network_directory,"Versie_score_verschillen.csv"))
  
  print("Gemiddelde score versie C en A verschillen significant")
  profvis::pause(20)
}
}

if (nrv == 4) {
  
  ttest2 <- tsum.test(mean.x=versie_score$mean[1],   s.x=versie_score$sd[1], n.x=versie_score$n[1],
                      mean.y=versie_score$mean[3], s.y=versie_score$sd[3], n.y=versie_score$n[3])
  
  if(ttest2$p.value < 0.05) {
    write.csv2(versie_score, paste0(Network_directory,"Versie_score_verschillen.csv"))
    
    print("Gemiddelde score versie C en A verschillen significant")
    profvis::pause(20)
  }
  
ttest3 <- tsum.test(mean.x=versie_score$mean[1],   s.x=versie_score$sd[1], n.x=versie_score$n[1],
                    mean.y=versie_score$mean[4], s.y=versie_score$sd[4], n.y=versie_score$n[4])


if(ttest3$p.value < 0.05) {
  write.csv2(versie_score, paste0(Network_directory,"Versie_score_verschillen.csv"))
  
  print("Gemiddelde score versie D en A verschillen significant")
  profvis::pause(20)
  
}
}
