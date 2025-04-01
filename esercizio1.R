library(readxl)
library(dplyr)

file_path = "C:/Users/Francesco/Documents/R/esercizi.xlsx"
tab_pdl = read_excel(file_path, sheet = "tab_pdl")

tab_pdl$data_esito = as.Date(tab_pdl$data_esito, format = "%Y-%m-%d")
tab_pdl = arrange(tab_pdl, id_anagrafica, data_esito)

tab_stato_pdl = data.frame(id_anagrafica = integer(), 
                           esito = character(), 
                           data_cambio_esito = as.Date(character()), 
                           codice_esito = integer(),
                           stringsAsFactors = FALSE)

id_anagrafica = NA
esito = NA
data_cambio_esito = as.Date(NA)
codice_esito = NA

temp_list = list()
for (i in 1:nrow(tab_pdl)) {
  id_anagraficanew = tab_pdl$id_anagrafica[i]
  esitonew = tab_pdl$esito[i]
  data_cambio_esitonew = tab_pdl$data_esito[i]
  
  if (!is.na(id_anagrafica) && id_anagrafica != id_anagraficanew) {
    temp_list[[length(temp_list) + 1]] = data.frame(id_anagrafica, esito, data_cambio_esito, codice_esito)
  }
  
  if (is.na(id_anagrafica) || id_anagrafica != id_anagraficanew) {
    id_anagrafica = id_anagraficanew
    esito = esitonew
    data_cambio_esito = data_cambio_esitonew
    codice_esito = ifelse(esitonew == "P", 1, 3)
  } else if (esito != esitonew) {
    esito = esitonew
    data_cambio_esito = data_cambio_esitonew
    codice_esito = ifelse(esitonew == "P", 1, 2)
  }
}

if (!is.na(id_anagrafica)) {
  temp_list[[length(temp_list) + 1]] = data.frame(id_anagrafica, esito, data_cambio_esito, codice_esito)
}

tab_stato_pdl = do.call(rbind, temp_list)

write.table(tab_stato_pdl, "C:/Users/Francesco/Documents/R/tab_stato_pdl.csv", row.names = FALSE, sep = ",", quote = FALSE)