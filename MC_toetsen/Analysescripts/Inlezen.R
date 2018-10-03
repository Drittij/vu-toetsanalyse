
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
  
  bestanden <-list(paste0(Network_directory,databestand1), paste0(Network_directory,databestand2))
  
  inleesfunctie <- function(x){
   data.table:: fread(x)
  }
  
  # ## Gebruik eventueel deze inleesfuntie als de bestanden eerder bewerkt zijn
  # inleesfunctie <- function(x){
  #   read.delim(x)
  # }
  # 
  teleformdata <- map_df(bestanden ,inleesfunctie)
  
  write.table(teleformdata, paste0(Network_directory,databestand_new), row.names = F, sep="\t")
  
  rm(teleformdata, samenvoegen)
  
  teleformdata <- read.table(paste0(Network_directory,databestand), sep="\t", header = T)

} else {
  ##Open databestand
  teleformdata <- data.table:: fread(paste0(Network_directory,databestand))
  # teleformdata <- read.delim(paste0(Network_directory,databestand)) %>% map_df(as.character)

}

teleformdata <- teleformdata %>%
  dplyr:: select(stud_nr, stud_naam, everything())

## Check of er dubbele studentnummers in voorkomen
if (anyDuplicated(teleformdata$stud_nr) > 0) {
  
  write.csv2("Er komen dubbele studentnummers voor, check de ruwe data", paste0(Network_directory,"error.csv"))
  
    stop(print("dubbele studentnummers"))
}
