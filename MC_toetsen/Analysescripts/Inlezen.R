
# Inlezen teleform en invullen metadata -----------------------------------

toetsinfo <- read_xlsx(paste0(Network_directory,"toetsinfo.xlsx"))
databestand <- toetsinfo$databestand
naamtoets <- toetsinfo$`naam toets`
datum <- format(as.Date(toetsinfo$`datum toets`), "%d-%m-%Y") 
nrq <- toetsinfo$`aantal vragen`
nra <- toetsinfo$`aantal antwoordalternatieven`
cesuur <- toetsinfo$cesuur
nrv <- toetsinfo$`aantal versies`
heranalyse <- toetsinfo$heranalyse
vakcode <- gsub("_ruwedata.DEL", "", databestand)

if (toetsinfo$samenvoegen == "y") {
  ## Samenvoegen 4 en 6k formulieren
  samenvoegen <- read_xlsx(paste0(Network_directory,"samenvoegen.xlsx"))
  databestand1 <- samenvoegen$databestand1
  databestand2 <- samenvoegen$databestand2
  databestand_new <- samenvoegen$databestand_new
  
  teleformdata1 <- read.csv2(paste0(Network_directory,databestand1), sep="\t", fileEncoding="UTF-8-BOM")
  teleformdata2 <- read.csv2(paste0(Network_directory,databestand2), sep="\t", fileEncoding="UTF-8-BOM")
  
  # teleformdata1 <- read.delim(paste0(Network_directory,databestand1))
  # teleformdata2 <- read.delim(paste0(Network_directory,databestand2))
  
  teleformdata <- bind_rows(teleformdata1, teleformdata2)
  
  write.table(teleformdata, paste0(Network_directory,databestand_new), row.names = F, sep="\t")
  
  rm(teleformdata, teleformdata1, teleformdata2, samenvoegen)
  
  teleformdata <- read.table(paste0(Network_directory,databestand), sep="\t", header = T)

} else {
  ##Open databestand
  teleformdata <- read.csv2(paste0(Network_directory,databestand), sep="\t", fileEncoding="UTF-8-BOM")
  
}

teleformdata <- teleformdata %>%
  dplyr:: select(stud_nr, stud_naam, everything())

## Check of er dubbele studentnummers in voorkomen
if (anyDuplicated(teleformdata$stud_nr) > 0) {
  
  write.csv2("Er komen dubbele studentnummers voor, check de ruwe data", paste0(Network_directory,"error.csv"))
  
    stop(print("dubbele studentnummers"))
}
