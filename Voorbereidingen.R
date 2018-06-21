################################################################################
### Voorbereidingen.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Voorbereidingen.R
### Doel: De working directory wordt bepaald door de locatie van het project 
### (vu-toetsanalyse)
### De specifieke functies en libraries voor dit project worden ingeladen
###
### Afhankelijkheden: geen
###
### Gebruikte datasets: geen
###
### Opmerkingen: geen
### 
################################################################################
### TODO:
### 1) Geen
###
################################################################################    
### Geschiedenis:
### 06-03-2018: DD: Aanmaken bestand
### 21-06-2018: DD: Functies toegevoegd om data te prepareren en analyseren
################################################################################

# installeren benodigde packages ------------------------------------------
if(!require(CTT)){install.packages("CTT")}
if(!require(stringr)){install.packages("stringr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(psych)){install.packages("psych")}
if(!require(schoRsch)){install.packages("schoRsch")}
if(!require(svDialogs)){install.packages("svDialogs")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(jmv)){install.packages("jmv")}
if(!require(readxl)){install.packages("readxl")}
if(!require(purrr)){install.packages("purrr")}
if(!require(knitr)){install.packages("knitr")}
if(!require(xlsx)){install.packages("xlsx")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(subscore)){install.packages("subscore")}
if(!require(difNLR)){install.packages("difNLR")}
if(!require(BSDA)){install.packages("BSDA")}
if(!require(ggrepel)){install.packages("ggrepel")}


# laden libraries ---------------------------------------------------------

library(stringr)
library(dplyr)
library(psych)
library(schoRsch) 
library(svDialogs)
library(ggplot2)
library(jmv)
library(readxl)
library(purrr)
library(knitr)
library(xlsx)
library(reshape2)
library(CTT)
library(tidyverse)
library(subscore)
library(difNLR)
library(BSDA)
library(ggrepel)

# Bepaal de netwerk directory op basis van het besturingsssyteem: windows = VU
Network_directory_WIN <- "G:/DSZ/OKZ/OTIR/Toetsen/Werkmap/"
Network_directory_MAC <- "/Volumes/groups/DSZ/OKZ/OTIR/Toetsen/Werkmap/"

if (.Platform$OS.type == "windows") {
  Network_directory <- Network_directory_WIN
} else {
  Network_directory <- Network_directory_MAC 
}

Network_directory


# Functie om vragen na te kijken (met meerdere antwoorden goed) -----------

score_mc <- function (items, key, output.scored = TRUE, ID = NA, rel = TRUE, 
                          multiKeySep = "none", multiKeyScore = c("or", "dich")) 
{
  t <- as.vector(ID)
  t <- table(ID)
  if (any(t > 1)) {
    for (i in 1:length(ID)) {
      for (j in 1:nrow(subset(t, t > 1))) {
        if (ID[i] == (rownames(subset(t, t > 1)))[j]) {
          ID[i] <- paste(ID[i], "/", i)
        }
      }
    }
    warning("Duplicate ID exists; the duplicate ID has been renamed and retained in the calculation")
  }
  if (!missing(ID)) {
    if (length(ID) == nrow(items)) 
      rownames(items) <- ID
    else warning("The length of ID vector does not match the sample size.")
  }
  if (missing(key)) {
    warning("No key provided, assuming pre-scored data.")
    scored <- apply(items, 2, function(XXX) {
      if (!is.numeric(XXX)) 
        XXX <- as.numeric(XXX)
      XXX
    })
  }
  else {
    if (length(key) == ncol(items)) {
      if (multiKeySep == "none") {
        scored <- t(apply(items, 1, function(X) {
          ifelse(X == (key), 1, 0)
        }))
      }
      else {
        scored <- array(0, dim = dim(items))
        key <- purrr:: map_df(key, as.character)
        items <- purrr:: map_df(items, as.character) %>% as.data.frame()
        for (colcol in 1:ncol(items)) {
          thisKey <- strsplit(key[[colcol]], multiKeySep)[[1]]
          thisAnswer <- strsplit(items[, colcol], multiKeySep)
          thisScore <- lapply(thisAnswer, function(XXX, 
                                                   myKey = thisKey) {
            compare <- XXX %in% myKey
            oot <- all(c(compare, compare)) * 1
            oot
          })
          scored[, colcol] <- unlist(thisScore)
        }
      }
    }
    else stop("Number of items is not equal to the length of key.")
  }
  scores <- rowSums(scored)
  names(scores) <- paste("P", c(seq(1:nrow(items))), sep = "")
  if (!rel == FALSE) 
    reli <- CTT:: reliability(scored)
  if (output.scored == FALSE & rel == FALSE) 
    out <- list(score = scores)
  if (output.scored == FALSE & rel == TRUE) 
    out <- list(score = scores, reliability = reli)
  if (output.scored == TRUE & rel == FALSE) 
    out <- list(score = scores, scored = scored)
  if (output.scored == TRUE & rel == TRUE) 
    out <- list(score = scores, reliability = reli, scored = scored)
  out
}



# Functie om teleformdata met 2 versies te prepareren ---------------------

prep_mc_2 <- function (teleformdatabestand, aantal_vragen = NULL, aantal_alternatieven = NULL) 
{
  
  ## Zet data in goede volgorde
  teleformdata <- teleformdata %>%
    dplyr:: select(stud_nr, stud_naam, everything())
  
  
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
  # volgorde <- read_xlsx(paste0(Network_directory,"Volgordeomzetting.xlsx")) %>% map_df(as.integer)
  orderB <- as.vector(volgorde$Bversie)

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
  
  list(teleformdata_correct = teleformdata_correct, data = data, sleutel = sleutel, gokkans = gk, student_versies = student_versies)
}


# Functie om data van een tentamen met 2 versies te analyseren ------------
analyze_2 <- function (data, sleutel, teleformdata_correct, aantal_vragen, cesuur, gokkans, student_versies) 
{
  
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
  
  total_score <- dplyr::rename(total_score, studentnummers = stud_nr)
  
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
  for ( i in 1:nrq ) itemanalyse$Rel_P[i] <- ((-1/(gokkans-1))*itemanalyse$P_waarde[i]+1-(-1/(gokkans-1)))
  
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
  
  for ( i in 1:nrq ) if( (itemanalyse$P_waarde[i] < (gokkans+0.04))&(itemanalyse$rir[i] > 0.05) ){
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
  
  ##Verwijder NA's uit itemanalyse
  if (nra == 2) {
    itemanalyse[,8:12] <- sapply(itemanalyse[,8:12], as.character)
    itemanalyse[,8:12][is.na(itemanalyse[,8:12])] <- " "
  }
  
  if (nra == 3) {
    itemanalyse[,9:13] <- sapply(itemanalyse[,9:13], as.character)
    itemanalyse[,9:13][is.na(itemanalyse[,9:13])] <- " "
  }
  
  if (nra == 4) {
    itemanalyse[,10:14] <- sapply(itemanalyse[,10:14], as.character)
    itemanalyse[,10:14][is.na(itemanalyse[,10:14])] <- " "
  }
  
  if (nra == 5) {
    itemanalyse[,11:15] <- sapply(itemanalyse[,11:15], as.character)
    itemanalyse[,11:15][is.na(itemanalyse[,11:15])] <- " "
  }
  
  
  if (nra == 6) {
    itemanalyse[,12:16] <- sapply(itemanalyse[,12:16], as.character)
    itemanalyse[,12:16][is.na(itemanalyse[,12:16])] <- " "
  }
  
  ## Voeg gebruikte sleutel toe aan itemanalyse
  tsleutel <- as.data.frame(t(sleutel))
  itemanalyse <- cbind(tsleutel, itemanalyse) %>% 
    dplyr:: rename(Key = V1)
  
  itemanalyse <- dplyr:: mutate(itemanalyse, itemName = colnames(sleutel))

  ##Bereken gemiddelde score en sd per toetsversie
  versie_score <- inner_join(total_score, student_versies, by = "studentnummers") %>% group_by(Toetsversie) %>%
    summarise(mean=mean(score), sd=sd(score), n=n())
  
  ttest <- tsum.test(mean.x=versie_score$mean[1],   s.x=versie_score$sd[1], n.x=versie_score$n[1],
                     mean.y=versie_score$mean[2], s.y=versie_score$sd[2], n.y=versie_score$n[2])
  
  try(if(ttest$p.value < 0.05) stop("Gemiddelde score versies verschillen significant"))
  
  list(itemanalyse = itemanalyse, toetswaarden = toets,  cijfers = total_score)
  
}