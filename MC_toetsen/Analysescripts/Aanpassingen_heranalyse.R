# 3. Invoeren aanpassingen heranalyse -------------------------------------
## Upload eventueel aangepaste/nieuwe sleutel
## Om meerdere antwoorden goed te rekenen lever komma gescheiden aan
## LET OP!! Verwijder ook de verwijderde vragen

if (heranalyse == "y") {
  sleutel <- read.csv2(paste0(Network_directory,"sleutel_nieuw.csv"))
  data <- dplyr:: select(data, one_of(names(sleutel)))
  
  nrq <- ncol(sleutel)
  nrc <- nrq+2
  
  ## Schrijf data weg
  write.csv2(data, file=paste0(Network_directory,"data.csv"), row.names=FALSE)
  
  # sleutel <- readxl:: read_xlsx(paste0(Network_directory,"sleutel_nieuw.xlsx"))
}

