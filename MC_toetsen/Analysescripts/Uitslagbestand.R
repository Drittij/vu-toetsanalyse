
# Vullen uitslagbestand ---------------------------------------------------

cijfers <- read.csv2(paste0(Network_directory,"results_student.csv")) %>% 
  dplyr:: select(-cijfer) %>% dplyr:: arrange(studentnamen) %>%  
  dplyr:: select(studentnamen, studentnummers, score)

instellingen <- data.frame(nrq, cesuur)

wb <- loadWorkbook("T:\\tentamens\\tentamenuitslag_R.xlsx", create = TRUE)
writeWorksheet(wb, instellingen, sheet = "transformatie", startRow = 2, startCol = 9, header = F)
writeWorksheet(wb, cijfers, sheet = "cijfers", startRow = 2, header = F)
setForceFormulaRecalculation(wb, sheet = "transformatie", TRUE)
setForceFormulaRecalculation(wb, sheet = "cijfers", TRUE)

saveWorkbook(wb, paste0(Network_directory,"uitslagbestand.xlsx"))
