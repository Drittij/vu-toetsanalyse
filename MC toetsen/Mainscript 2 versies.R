################################################################################
### Mainscript 2 versies.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Mainscript 2 versies.R
### Doel: Startscript voor het inlezen en analyse van teleform data voor 
### tentamen met 2 versies en 4 antwoordalternatieven
### 
### Afhankelijkheden: geen
###
### Gebruikte datasets: .DEL bestanden
###
### Opmerkingen: geen
### 
################################################################################
### TODO:
### 1) Geen
###
################################################################################    
### Geschiedenis:
### 07-03-2018: DD: Aanmaken bestand
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
# teleformdata <- read.csv2(paste0(Network_directory,databestand), sep="\t", 
#                           fileEncoding="utf-16")
teleformdata <- read.csv2(paste0(Network_directory,databestand), sep="\t")

teleformdata <- teleformdata %>%
  dplyr:: select(stud_nr, stud_naam, everything())

################################################################################
## 2. MANIPULEREN
################################################################################
##Laden van transformatie script en errors tonen in console
ll <- parse(file = "MC toetsen/Toetsanalyse 2 versies.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

################################################################################
## 3. Maken rapport in pdf
################################################################################
## Maken van itemanalyse in pdf
thetitle=naamtoets; rmarkdown::render("MC toetsen/Itemanalyse.Rmd", 
                                      output_file = paste0(Network_directory,
                                                           "Itemanalyse.pdf"))

################################################################################
## 4. Extra functies
################################################################################
##Bereken scores per vraaggroep. 
## Pas onderstaand script aan welke vragen bij elkaar horen
newnames <- c("studentnummer", "studentnaam", vrn)
colnames(scored_datax) <- newnames
vraag_groep <- mutate(scored_datax, 
                      groep1=V1+V2+V3+V4, 
                      groep2=V5+V6+V7+V8, 
                      groep3=V9+V10+V11+V12)

vraag_groep_score <- dplyr:: select(vraag_groep, studentnummer, 
                                    groep1, groep2, groep3)

write.csv2(vraag_groep_score, file=paste0(Network_directory,
                                          "vraaggroepen.csv"), row.names=FALSE)

## Correlatiematrix vragen
par(mfrow=c(1,1))
corP <- psych:: polychoric(scored_data$scored) 
corrplot:: corrplot(corP$rho) 

################################################################################
## EINDE
################################################################################
## Clean workspace
rm(list = ls())

