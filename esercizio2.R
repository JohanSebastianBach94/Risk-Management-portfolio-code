library(readr)
library(readxl)
library(dplyr)
library(data.table)

file_path = "C:/Users/Francesco/Documents/R/esercizi.xlsx"
tab_ruoli = read_excel(file_path, sheet = "tab_ruoli")
tab_stato_pdl = read_csv("C:/Users/Francesco/Documents/R/tab_stato_pdl.csv")

tab_tot = merge(tab_ruoli, tab_stato_pdl, by = "id_anagrafica", all.x = TRUE)

tab_tot$ruolo = factor(tab_tot$ruolo, levels = c("CONTROPARTE", "COOBBLIGATO", "GARANTE"), ordered = TRUE)


tab_tot = as.data.table(tab_tot)
tab_ris = tab_tot[order(codice_esito, ruolo, -as.numeric(data_di_nascita)),.SD[1], by = id_pratica]
head(tab_ris)
tab_ris = tab_ris[codice_esito == 1 | (codice_esito == 2 & data_cambio_esito > as.Date("2021-06-30")), ]

write.table(tab_ris, "C:/Users/Francesco/Documents/R/risultato.csv", row.names = FALSE, sep = ",", quote = FALSE) 
