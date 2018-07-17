################################################################################
### Mainscript Heranalyse.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Mainscript Heranalyse.R
### Doel: Script voor het inlezen en analyseren van teleform data voor 
### tentamen met 2:6 antwoordalternatieven en 1:4 versies
### 
### Afhankelijkheden: del bestand, volgordeomzetting en toetsinfo
###
### Gebruikte datasets: ruwe data .DEL bestand, nieuwe sleutel in csv
###
### Opmerkingen: geen
### 
################################################################################
### TODO:
### 1)
###
################################################################################    
### Geschiedenis:
### 24-04-2018: DD: Aanmaken bestand
### 28-06-2018: DD: Script verplaatst naar subscripts
### 17-07-2018: DD: Input toetsvariabele verplaatst naar toetsinfo excel
################################################################################

# 0. Voorbereidingen ------------------------------------------------------
## Lees de packages, functies en libraries in
source("H:/Documents/github/vu-toetsanalyse/Voorbereidingen.R")
# LET OP: Bij meerdere versies bestand Volgordeomzetting.csv klaarzetten in map

# 1. Inlezen --------------------------------------------------------------
# Lees alle benodigde bestanden in:
# defineer naam bestand, datum, aantal vragen, gokkans en cesuur, aantal versies
# in toetsinfo excel bestand
source("H:/Documents/github/vu-toetsanalyse/MC_toetsen/Analysescripts/Inlezen.R")

# 2. Manipuleren ----------------------------------------------------------
# Data prepareren voor analyse, o.a. volgordeomzetting doorvoeren
source("H:/Documents/github/vu-toetsanalyse/MC_toetsen/Analysescripts/Manipuleren.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

# 3. Invoeren aanpassingen heranalyse -------------------------------------

##Verwijder vragen uit dataset (optioneel te gebruiken)
# data <- dplyr:: select(data, -V17)
# nrq <- 27
# nrc <- nrq+2
# 
# ## Schrijf data weg
# write.csv2(data, file=paste0(Network_directory,"data.csv"), row.names=FALSE)
# 
# ## Upload eventueel aangepaste/nieuwe sleutel
# ## Om meerdere antwoorden goed te rekenen lever komma gescheiden aan
# 
# ## LET OP!! Verwijder ook de verwijderde vragen
# sleutel <- read.csv2(paste0(Network_directory,"sleutel_nieuw.csv"))
# # sleutel <- readxl:: read_xlsx(paste0(Network_directory,"sleutel_nieuw.xlsx"))

# 4. Analyseren -----------------------------------------------------------
# Genereren itemanalyse en scores
source("H:/Documents/github/vu-toetsanalyse/MC_toetsen/Analysescripts/Analyseren.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

# 4A. Vul uitslagbestand --------------------------------------------------
source("H:/Documents/github/vu-toetsanalyse/MC_toetsen/Analysescripts/Uitslagbestand.R")


# 5. Genereren pdf rapport itemanalyse ------------------------------------
thetitle=naamtoets; rmarkdown::render("H:/Documents/github/vu-toetsanalyse/MC_toetsen/Analysescripts/Itemanalyse.Rmd", 
                                      output_file = paste0(Network_directory,
                                                           "Itemanalyse.pdf"))

# 6. Genereren inzage rapporten per student in pdf ------------------------
# source("H:/Documents/github/vu-toetsanalyse/MC_toetsen/Analysescripts/Inzage rapport.R")


# 7. Vraaggroepen ---------------------------------------------------------

##Bereken scores per vraaggroep. 
## Pas onderstaand script aan welke vragen bij elkaar horen
# newnames <- c("studentnummer", "studentnaam", vrn)
# colnames(scored_datax) <- newnames
# vraag_groep <- mutate(scored_datax, 
#                       groep1=V1+V2+V3+V4, 
#                       groep2=V5+V6+V7+V8, 
#                       groep3=V9+V10+V11+V12)
# 
# vraag_groep_score <- dplyr:: select(vraag_groep, 
#                                     studentnummer, 
#                                     groep1, 
#                                     groep2, 
#                                     groep3)
# 
# write.csv2(vraag_groep_score, file=paste0(Network_directory,
#                                           "vraaggroepen.csv"), row.names=FALSE)

# 8. Correlatiematrix vragen genereren ------------------------------------
# ## Correlatiematrix vragen
# par(mfrow=c(1,1))
# corP <- psych:: polychoric(scored_data$scored) 
# corrplot:: corrplot(corP$rho) 


# 9. Analyse van afleiders ------------------------------------------------
# (The point-biserial correlation between that reponse and the total score with that item removed)
# disanalyse <- distractorAnalysis(data, sleutel, multiKeySep=",",
#                                  multiKeyScore=c("or","dich"))

################################################################################
## EINDE
################################################################################
## Clean workspace
rm(list = ls())
