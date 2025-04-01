# Caricamento librerie
library(ggplot2)
library(dplyr)
library(lubridate)

# Creazione della cartella per i grafici (se non esiste)
dir.create("grafici", showWarnings = FALSE)

# Caricamento dati
file_path <- "C:/Users/Francesco/Documents/R/risultato.csv"
dati <- read.csv(file_path, stringsAsFactors = FALSE, sep = ";")
head(dati)

# Verifica formato data_di_nascita

dati$data_di_nascita <- dmy(dati$data_di_nascita, quiet = TRUE)
head(dati$data_di_nascita)

# Rimuovere eventuali righe con data non valida
dati <- dati[!is.na(dati$data_di_nascita), ]

# Calcolare l'età
dati$eta <- as.integer(floor(as.numeric(Sys.Date() - dati$data_di_nascita) / 365.25))

# Creare fasce di età
dati$fascia_eta <- cut(
  dati$eta,
  breaks = c(0, 20, 30, 40, 50, 60, 70, 80, 90, Inf),
  labels = c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
  include.lowest = TRUE
)

# Conteggio mutui
num_mutui <- sum(dati$ruolo == "CONTROPARTE") + sum(dati$ruolo == "COOBBLIGATO") / 2

# ---- GRAFICO 1: Distribuzione fasce d'età ----
grafico1 <- ggplot(filter(dati, ruolo %in% c("CONTROPARTE", "COOBBLIGATO")),
                   aes(x = fascia_eta, fill = ruolo)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribuzione delle fasce di età", x = "Fascia d'età", y = "Numero") +
  theme_minimal()

ggsave("grafici/distribuzione_fasce_eta.png", grafico1, width = 8, height = 6)

# ---- GRAFICO 2: Occupazione vs Disoccupazione (Percentuali) ----
dati_persolavoro <- data.frame(
  stato = c("Ha lavoro", "Perso lavoro"),
  conteggio = c(sum(dati$esito == "P", na.rm = TRUE), sum(dati$codice_esito == 2, na.rm = TRUE))
)

dati_persolavoro$percentuale <- round(100 * dati_persolavoro$conteggio / sum(dati_persolavoro$conteggio), 1)

grafico2 <- ggplot(dati_persolavoro, aes(x = "", y = conteggio, fill = stato)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(percentuale, "%")),
            position = position_stack(vjust = 0.5), size = 5, color = "white") +
  labs(title = "Occupazione vs Disoccupazione") +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

ggsave("grafici/occupazione_vs_disoccupazione.png", grafico2, width = 6, height = 6)

# ---- GRAFICO 3: Distribuzione lavoratori (Controparti e Garante) ----
grafico3 <- ggplot(filter(dati, esito == "P", ruolo %in% c("CONTROPARTE", "GARANTE")),
                   aes(x = ruolo, fill = ruolo)) +
  geom_bar() +
  labs(title = "Distribuzione lavoratori", x = "Ruolo", y = "Numero") +
  theme_minimal()

ggsave("grafici/distribuzione_lavoratori.png", grafico3, width = 6, height = 6)

# ---- GRAFICO 4: Età perdita occupazione controparti ----
dati_persolavoro_eta <- filter(dati, ruolo == "CONTROPARTE", codice_esito == 2)

# Assicurarsi che entrambe le colonne siano di tipo Date
dati_persolavoro_eta$data_cambio_esito <- as.Date(dati_persolavoro_eta$data_cambio_esito)
dati_persolavoro_eta$data_di_nascita <- as.Date(dati_persolavoro_eta$data_di_nascita)

# Calcolare l'età al momento della perdita di occupazione
dati_persolavoro_eta$eta_perdita <- as.integer(floor(as.numeric(dati_persolavoro_eta$data_cambio_esito - dati_persolavoro_eta$data_di_nascita) / 365.25))

# Controllo eventuali NA nell'età di perdita
table(is.na(dati_persolavoro_eta$eta_perdita))

# Rimuovere eventuali NA
dati_persolavoro_eta <- na.omit(dati_persolavoro_eta)

grafico4 <- ggplot(dati_persolavoro_eta, aes(x = eta_perdita)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Età perdita occupazione controparti", x = "Età", y = "Numero") +
  theme_minimal()

ggsave("grafici/eta_perdita_occupazione.png", grafico4, width = 6, height = 6)