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
################################################################################

## installeren benodigde packages
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
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(subscore)){install.packages("subscore")}
if(!require(difNLR)){install.packages("difNLR")}

## laden libraries
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
library(CTT)
library(tidyverse)
library(subscore)
library(difNLR)

# Bepaal de netwerk directory op basis van het besturingsssyteem: windows = VU
Network_directory_WIN <- "G:/DSZ/OKZ/OTIR/Toetsen/Werkmap/"
Network_directory_MAC <- "/Volumes/groups/DSZ/OKZ/OTIR/Toetsen/Werkmap/"

if (.Platform$OS.type == "windows") {
  Network_directory <- Network_directory_WIN
} else {
  Network_directory <- Network_directory_MAC 
}

Network_directory


## Functie om vragen na te kijken (met meerdere antwoorden goed)
score_daniel <- function (items, key, output.scored = TRUE, ID = NA, rel = TRUE, 
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
