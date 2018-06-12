################################################################################
### Mainscript Heranalyse 1 versie.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Mainscript Heranalyse 1 versie.R
### Doel: Startscript voor het inlezen en analyse van teleform data voor 
### tentamen met 2:6 antwoordalternatieven
### 
### Afhankelijkheden: geen
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
################################################################################

############################################################################
## 00 VOORBEREIDINGEN
############################################################################
## Lees de packages, functies en libraries in
source("Voorbereidingen.R")

################################################################################
## 1. INLEZEN
################################################################################
## Lees alle benodigde bestanden in:
##Verander hier de naam van het bestand dat je in wil lezen, 
## defineer aantal vragen, gokkans en cesuur
databestand <- dlgInput("Wat is de naam van het ruwe data bestand? ", 
                        Sys.info()["databestand"])$res
naamtoets <- dlgInput("Wat is de naam van de toets ", 
                      Sys.info()["naamtoets"])$res
datum <- dlgInput("Datum afname toets ", Sys.info()["datumtoets"])$res
nrq <- dlgInput("Hoeveel vragen bevat de toets? ", Sys.info()["nrq"])$res
nrq <- as.numeric(nrq)
nra <- dlgInput("Hoeveel antwoordalternatieven? ", Sys.info()["nra"])$res
nra <- as.numeric(nra)
cesuur <- dlgInput("Wat is de cesuur? ", Sys.info()["cesuur"])$res
cesuur <- as.numeric(cesuur)

##Open databestand
teleformdata <- read.csv2(paste0(Network_directory,databestand), sep="\t", fileEncoding="UTF-8-BOM")

teleformdata <- teleformdata %>%
  dplyr:: select(stud_nr, stud_naam, everything())

################################################################################
## 2. MANIPULEREN stap 1
################################################################################
##Laden van transformatie script en errors tonen in console
ll <- parse(file = "MC toetsen/Heranalyse Toetsanalyse 1 versie stap 1.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

################################################################################
## 3. Aanpassingen heranalyse
################################################################################

##Verwijder vragen uit dataset (optioneel te gebruiken)
data <- dplyr:: select(data, -V38)
nrq <- 39
nrc <- nrq+2

## Schrijf data weg
write.csv2(data, file=paste0(Network_directory,"data.csv"), row.names=FALSE)

## Upload eventueel aangepaste/nieuwe sleutel
## Om meerdere antwoorden goed te rekenen lever komma gescheiden aan

## LET OP!! Verwijder ook de verwijderde vragen
sleutel <- read.csv2(paste0(Network_directory,"sleutel_nieuw.csv"))
# sleutel <- readxl:: read_xlsx(paste0(Network_directory,"sleutel_nieuw.xlsx"))

################################################################################
## 4. ANALYSEREN stap 2
################################################################################
##Laden van transformatie script en errors tonen in console
ll <- parse(file = "MC toetsen/Heranalyse Toetsanalyse 1 versie stap 2.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

################################################################################
## 5. Maken rapport in pdf
################################################################################
## Maken van itemanalyse in pdf
thetitle=naamtoets; rmarkdown::render("MC toetsen/Itemanalyse.Rmd", 
                                      output_file = paste0(Network_directory,
                                                           "Itemanalyse.pdf"))

# thetitle=naamtoets; rmarkdown::render("MC toetsen/Itemanalyse.Rmd", 
#                                       output_file = paste0(Network_directory,
#                                                            "Itemanalyse.html"))


################################################################################
## 6. Maken inzagerapport per student in pdf
################################################################################
# source("MC toetsen/Inzage rapport.R")

################################################################################
## 7. Extra functies
################################################################################
##Bereken scores per vraaggroep. 
## Pas onderstaand script aan welke vragen bij elkaar horen
newnames <- c("studentnummer", "studentnaam", vrn)
colnames(scored_datax) <- newnames
vraag_groep <- mutate(scored_datax, 
                      groep1=V1+V2+V3+V4, 
                      groep2=V5+V6+V7+V8, 
                      groep3=V9+V10+V11+V12)

vraag_groep_score <- dplyr:: select(vraag_groep, 
                                    studentnummer, 
                                    groep1, 
                                    groep2, 
                                    groep3)

write.csv2(vraag_groep_score, file=paste0(Network_directory,
                                          "vraaggroepen.csv"), row.names=FALSE)

## Correlatiematrix vragen
par(mfrow=c(1,1))
corP <- psych:: polychoric(scored_data$scored) 
corrplot:: corrplot(corP$rho) 

## Analyse A-waarden (The point-biserial correlation between that reponse 
## and the total score with that item removed) - we
disanalyse <- distractorAnalysis(data, sleutel, multiKeySep=",",
                                 multiKeyScore=c("or","dich"))

################################################################################
## EINDE
################################################################################
## Clean workspace
rm(list = ls())

