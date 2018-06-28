################################################################################
### Inzage rapport.R
################################################################################
### R code voor Tentamenanalyse Vrije Universiteit Amsterdam
###
### Bestandsnaam: Inzage rapport.R
### Doel: Script om een inzage rapport per student te maken van een mc tentamen
### 
### Afhankelijkheden: Mainscript 1 versie.R
###
### Gebruikte datasets: 
###
### Opmerkingen: geen
### 
################################################################################
### TODO:
### 1) Geen
###
################################################################################    
### Geschiedenis:
### 12-03-2018: DD: Aanmaken bestand
################################################################################

################################################################################
## 1. Maken rapport in pdf
################################################################################

for(i in 1:nrow(teleformdata_new)) {
  student <- teleformdata_new[i,"stud_nr"]

  teleformdata_new_selectie <- filter(teleformdata_new, 
                                        `stud_nr`==student)
  
  total_score_selectie <- filter(total_score, 
                                      `studentnummers`==student)
  
  thetitle=paste("Inzage toets", naamtoets); rmarkdown::render("MC toetsen/Analysescripts/Inzage rapport.Rmd", 
                      output_file = paste0(Network_directory,"/inzage/", student,".pdf"))
  
}


