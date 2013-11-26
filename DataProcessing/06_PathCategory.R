###############################################
# Fuege Wegkategorie hinzu
# in: EventsInRaster_TimeWeather.rds, Wegkategorien.csv
# out: EventsInRaster_TimeWeatherPath.rds
# by: Heidi
###############################################
# setwd("Z:/Dropbox/Consulting/Daten/code")
library(plyr)

dat <- readRDS("../EventsInRaster_TimeWeather.rds")
cat0 <- read.csv2("../Wegkategorien.csv")
cat <- cat0[,c("name", "standortnr", "Wegkategorie")]
names(cat) <- c("siten", "sitenr", "category")
cat$site <- paste(cat$sitenr, cat$siten, sep = "-") 
cat$category <- factor(cat$category)#, labels = c("trail", "forest_road", "tarred_road", "ww"))
cat$siten <- NULL
cat$sitenr <- NULL

d <- merge(dat, cat, by = "site", all.x = TRUE)

saveRDS(d, "../EventsInRaster_TimeWeatherPath.rds")