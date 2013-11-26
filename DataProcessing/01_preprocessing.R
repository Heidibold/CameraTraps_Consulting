###########################################
# Datensatz vorbereiten
# - Varible Zeit als POSIXct ohne Sommerzeit
# - Kammeranummern richtig
# - nicht sinnvolle Standorte entfernen
# - Variable Foto erstellen
# in: 20131025_Export-2009_2012_KW.csv
# out: Basis_Datensatz.rds
# by: Heidi
###########################################

# setwd("Z:/Dropbox/Consulting/Daten")
# setwd("/home/heidi/Dropbox/Consulting/Daten")
library(ggplot2)
library(plyr)
# data <- read.csv2(file = "20130723_Export 2009_2012.csv", header = TRUE)
# data <- read.csv2(file = "20131025_Export-2009_2012_KW.csv", header = TRUE)
data <- read.csv2(file = "../20131008_export_2009_2012_D_ready.csv", header = TRUE)

# str(data)
# table(data$Motiv, data$Standortname)
# ggplot(data, aes(Standortname, fill=Motiv)) + geom_bar()


### Zeit so einstellen, dass immer auf Winterzeit (also Sommerzeit - 1h)
data$ZeitMitSommerzeit <- paste(data$Datum, data$Uhrzeit)
head(data.frame(data$Zeit, data$Datum, data$Uhrzeit))
data$ZeitMitSommerzeit <- as.POSIXct(data$ZeitMitSommerzeit, tz = "", "%d.%m.%Y %H:%M:%S")
data$ZeitUTC <- .POSIXct(data$ZeitMitSommerzeit, tz = "GMT")
data$Zeit <- data$ZeitUTC + 60*60


# ## Wie viele Kameras gibt es pro Standort? Wie viele Photos haben diese Kameras geschossen?
# CamTable <- list()
# for(i in seq_along(unique(data$Standortname))) {
#   d <- data[data$Standortname == unique(data$Standortname)[i], ]
#   CamTable[[i]] <- table(d$Kameranummer)
# }
# names(CamTable) <- unique(data$Standortname)
# 
# ## Standorte, an denen nicht zwei Kameras stehen
# nottwo <- sapply(CamTable, length) != 2
# CamTable[nottwo]
# 
# ggplot(data[data$Standortname == "21-Wolfsriegel", ], aes(Zeit, Kameranummer)) + geom_point()
# ggplot(data[data$Standortname == "8-Lusenhaenge", ], aes(Zeit, Kameranummer)) + geom_point()

## Kammeranummern bei Standort 21 und 8 anpassen
data$Kameranummer[data$Kameranummer == 55 & !is.na(data$Kameranummer)] <- 41
data$Kameranummer[data$Kameranummer == 56 & !is.na(data$Kameranummer)] <- 42
data$Kameranummer[data$Kameranummer == 57 & !is.na(data$Kameranummer)] <- 15
data$Kameranummer[data$Kameranummer == 58 & !is.na(data$Kameranummer)] <- 16


## Wie oft fehlt die Information Standortname?
sum(is.na(data$Standortname))
## Wie oft fehlt die Information Kameranummer?
sum(is.na(data$Kameranummer))

## Standorte aus Datensatz entfernen: 23-Buchenstieg, 28-TFG I (AG), 29-TFG II (EG)
dt <- data[!(data$Standortname %in% c("23-Buchenstieg", "28-TFG I (AG)", "29-TFG II (EG)")), ] 
dt$Standortname <- factor(dt$Standortname)



## Variable Foto erstellen
dt$Foto <- character(length = nrow(dt))
# Tiere
RnumbTier <- which(dt$Tierart != "")
RnumbTierUnbek <- which(dt$Tierart == "" & dt$Motiv == "Tier")
dt$Foto[RnumbTier] <- as.character(dt$Tierart[RnumbTier])
dt$Foto[RnumbTierUnbek] <- "TierUnbekannt"

# Menschen
RnumbMensch <- which(dt$Motiv == "Mensch")
dt$Foto[RnumbMensch] <- paste0(dt$Motiv[RnumbMensch], dt$Mensch_Detail[RnumbMensch])
dt$Foto[dt$Foto == "MenschHund" & !is.na(dt$Foto)] <- "Mensch"

# Sonstiges
RnumbSonstiges <- which(dt$Motiv == "Sonstiges" | dt$Sonstiges != "")
komisch <- which(dt$Motiv == "Mensch" & dt$Sonstiges != "")
dt$Foto[RnumbSonstiges] <- "Sonstiges"
dt$Foto[komisch] <- "Mensch"

# NA
dt$Foto[dt$Foto == ""] <- NA


# Fototab <- as.data.frame(table(dt$Foto))
# leer <- which(dt$Foto == "")
# table(dt$Motiv[leer])

## nur relevante Fotos ausw?hlen
# paste(levels(dt$Foto), collapse = "', '")
# dt <- dt[dt$Foto %in% c('Fuchs', 'Hase', 'Hauskatze', 'Luchs', 'Marder', 'Mensch', 'MenschAuto', 'Rehwild', 'Rotwild', 'Wildschwein'), ]
dt$Foto <- factor(dt$Foto)

saveRDS(dt, "Basis_Datensatz.rds")