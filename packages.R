################################################################################
### Gebruik functies.R 
################################################################################
### R code voor Student Analytics Vrije Universiteit Amsterdam
### Copyright 2018 VU
### Web Page: http://www.vu.nl
### Contact: Theo Bakker (t.c.bakker@vu.nl)
###
### Bestandsnaam: Gebruik functies.R 
### Doel: Dit script maakt inzichtelijk welk script welke functies gebruikt,
### en welke packages hiervoor gebruikt worden
###
### Afhankelijkheden: Main script.R
###
### Gebruikte datasets: Datasets
###
### Opmerkingen: 
### 1) Geen
###
################################################################################
### TODO:
### 1) Geen
###
################################################################################    
### Geschiedenis:
### 17-08-2017: JvZ: Aanmaak bestand
################################################################################

## Maak een lijst van alle R files
scripts <- list.files(pattern = ".R$", recursive = TRUE)


for(filename in scripts){
  print(filename)
  ## parse hest best
  tryCatch(tmp <- getParseData(parse(filename, keep.source=TRUE)),
           error = function(e) message(as.character(e)))
  ## haal alle functienamen uit het script
  nms <- tmp$text[which(tmp$token=="SYMBOL_FUNCTION_CALL")]
  ## maak een df met alle functies en de bestandsnaam
  if(length(nms) > 0){
    df_tmp <- data.frame(nms = nms, filename = filename)
  }
  ## voeg df_tmp samen met df_totaal
  if(exists("df_totaal") & exists("df_tmp")){
    df_totaal <- unique(rbind(df_totaal, df_tmp))
    rm(df_tmp)
  } else if(exists("df_tmp")){
    df_totaal <- df_tmp
  }
}

## sorteer alle unieke functies
funs <- unique(sort(as.character(df_totaal$nms)))
## bepaal wat de herkomst van deze functie is
src <- paste(as.vector(sapply(funs, find)))
## voeg dit samen naar een data frame
Gebruikte_functies <- data.frame(nms = funs, src = src)
## merge dit data frame zodat per script duidelijk wordt 
Gebruikte_functies_scripts <- merge(Gebruikte_functies, df_totaal, all.x =T)

## Maak hier een geaggregeerde tabel
functies <- Gebruikte_functies_scripts %>% 
  group_by(nms, src) %>% 
  summarise(Aantal_scripts = n())

packages_in_gebruik <- functies %>% 
  group_by(src) %>% 
  summarise(Aantal_scripts = sum(Aantal_scripts))



