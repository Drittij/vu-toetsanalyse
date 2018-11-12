
# Vullen uitslagbestand ---------------------------------------------------
total_score$studentnamen <- trimws(total_score$studentnamen)

cijfers <- total_score %>% 
  dplyr:: select(-cijfer) %>% dplyr:: arrange(studentnamen) %>%  
  dplyr:: select(studentnamen, studentnummers, score)

instellingen <- data.frame(nrq, cesuur)

wb <- loadWorkbook("helpfiles/tentamenuitslag_R.xlsx", create = TRUE)
writeWorksheet(wb, instellingen, sheet = "transformatie", startRow = 2, startCol = 9, header = F)
writeWorksheet(wb, cijfers, sheet = "cijfers", startRow = 2, header = F)
setForceFormulaRecalculation(wb, sheet = "*", TRUE)
clearRange(wb, sheet = "cijfers", coords = c(nrst+2, 6, 1000, 6))
clearRange(wb, sheet = "cijfers", coords = c(nrst+2, 4, 1000, 4))

saveWorkbook(wb, paste0(Network_directory,vakcode,"_","uitslagbestand.xlsx"))
