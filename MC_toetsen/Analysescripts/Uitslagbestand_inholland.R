
# Vullen uitslagbestand ---------------------------------------------------
total_score$studentnamen <- trimws(total_score$studentnamen)

cijfers <- total_score %>% 
  dplyr:: select(-cijfer) %>% dplyr:: arrange(studentnamen) %>%  
  dplyr:: select(studentnamen, studentnummers, score)

klas <- readxl:: read_excel("G:/DSZ/OKZ/OTIR/tentamens/inholland_klassen.xlsx") %>% 
  rename(studentnummers = nr)

cijfers <- cijfers %>% 
  left_join(klas) %>% 
  dplyr:: select(studentnamen, studentnummers, Klas, score) %>% 
  distinct(studentnummers, .keep_all = T)

instellingen <- data.frame(nrq, cesuur)

wb <- loadWorkbook("helpfiles/tentamenuitslag_R - inholland.xlsx", create = TRUE)
writeWorksheet(wb, instellingen, sheet = "transformatie", startRow = 2, startCol = 9, header = F)
writeWorksheet(wb, cijfers, sheet = "cijfers", startRow = 2, header = F)
setForceFormulaRecalculation(wb, sheet = "*", TRUE)
clearRange(wb, sheet = "cijfers", coords = c(nrst+2, 7, 1000, 7))
clearRange(wb, sheet = "cijfers", coords = c(nrst+2, 5, 1000, 5))

saveWorkbook(wb, paste0(Network_directory,vakcode,"_","uitslagbestand.xlsx"))
