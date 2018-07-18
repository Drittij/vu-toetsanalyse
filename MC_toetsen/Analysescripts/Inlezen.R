
# Inlezen teleform en invullen metadata -----------------------------------

# databestand <- dlgInput("Wat is de naam van het ruwe data bestand? ", 
#                         Sys.info()["databestand"])$res
# naamtoets <- dlgInput("Wat is de naam van de toets ", 
#                       Sys.info()["naamtoets"])$res
# datum <- dlgInput("Datum afname toets ", Sys.info()["datumtoets"])$res
# nrq <- dlgInput("Hoeveel vragen bevat de toets? ", Sys.info()["nrq"])$res
# nrq <- as.numeric(nrq)
# nra <- dlgInput("Hoeveel antwoordalternatieven? ", Sys.info()["nra"])$res
# nra <- as.numeric(nra)
# cesuur <- dlgInput("Wat is de cesuur? ", Sys.info()["cesuur"])$res
# cesuur <- as.numeric(cesuur)
# nrv <- dlgInput("Aantal tentamenversies? ", Sys.info()["nrv"])$res
# nrv <- as.numeric(nrv)

toetsinfo <- read_xlsx(paste0(Network_directory,"toetsinfo.xlsx"))
databestand <- toetsinfo$databestand
naamtoets <- toetsinfo$`naam toets`
datum <- toetsinfo$`datum toets`
nrq <- toetsinfo$`aantal vragen`
nra <- toetsinfo$`aantal antwoordalternatieven`
cesuur <- toetsinfo$cesuur
nrv <- toetsinfo$`aantal versies`
heranalyse <- toetsinfo$heranalyse

##Open databestand
teleformdata <- read.csv2(paste0(Network_directory,databestand), sep="\t", fileEncoding="UTF-8-BOM")

teleformdata <- teleformdata %>%
  dplyr:: select(stud_nr, stud_naam, everything())