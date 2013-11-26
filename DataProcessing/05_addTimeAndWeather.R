###############################################
# Fuege Zeit und Wetterinfo zu Datensatz hinzu
# in: EventsInRaster.rds, Wetterdaten.xls
# out: EventsInRaster_TimeWeather.rds
# by: Heidi
###############################################

# setwd("Z:/Dropbox/Consulting/Daten/code")
# setwd("/home/heidi/Dropbox/Consulting/Daten")

er <- readRDS("../EventsInRaster.rds")


#######################################################
## Zeitvariablen erstellen
er$time <- as.POSIXlt(er$time)
er$hour <- er$time$hour
er$yday <- er$time$yday
er$wday <- er$time$wday
er$wday <- factor(er$wday, labels = c("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"))
er$month <- er$time$mon + 1
er$year <- er$time$year + 1900
er$date <- as.Date(trunc(er$time, "days"))


#######################################################
## Kovariablen zu Wetter hinzufuegen

library(xlsx)
wetter <- read.xlsx("../Wetterdaten.xls", sheetIndex = 1, 
                    startRow = 5, header = TRUE)
wetter <- wetter[, c("DATUM", "TM", "SH")]
colnames(wetter) <- c("datum", "temp", "snow")
wetter$snow <- as.character(wetter$snow)
wetter$snow[wetter$snow == "."] <- "0.5"
wetter$snow[is.na(wetter$snow)] <- "0.0"
wetter$snow <- as.numeric(wetter$snow)


er.wetter <- merge(er, wetter, by.x = "date", by.y = "datum", all.x = TRUE)

saveRDS(er.wetter, file = "../EventsInRaster_TimeWeather.rds")
