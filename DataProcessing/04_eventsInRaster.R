###########################################
# Erstelle Datensatz mit 5-Minuten-Abschnitten
# der Beobachtungszeit. Lege Events in diese
# Abschnitte
# - Finde Beobachtungszeitraum fuer jeden Standort (timeframes)
# - Runde beobachtungszeitraum (Minimum auf 5 Min drunter, Maximum auf 5 Min drüber)
# - Gehe durch alle Fotos/Motive und erhalte Events in 5-Minuten-Raster als Liste
# - Erstelle Datensatz mit Events in 5-Minuten Raster
# in: Basis_Datensatz.rds, Events_Info2.rds
# out: EventsInRaster_list.rds, EventsInRaster.rds
# by: Heidi
###########################################
# setwd("Z:/Dropbox/Consulting/Daten/code/")
# setwd("/home/heidi/Dropbox/Consulting/Daten/code")
library(ggplot2)
library(plyr)
dt <- readRDS("../Basis_Datensatz.rds")
events <- readRDS("../Events_Info2.rds")



#######################################################
## Finde Beobachtungszeitraum fuer jeden Standort (timeframes)
get_timeframe <- function(Kam){
  range(Kam$Zeit)
}
timeframes <- ddply(dt, .(Standortname), get_timeframe, .progress = "text") 
names(timeframes) <- c("Standortname", "on", "off")
timeframes$on <- as.POSIXlt(timeframes$on)
timeframes$off <- as.POSIXlt(timeframes$off)


## Runde beobachtungszeitraum (Minimum auf 5 Min drunter, Maximum auf 5 Min drüber)
round.floor <- function(x,base){ 
  base*floor(x/base) 
} 
timeframes$on$min <- round.floor(timeframes$on$min, 5)
round.ceiling <- function(x,base){
  base*ceiling(x/base)
}
timeframes$off$min <- round.ceiling(timeframes$off$min, 5)




#######################################################
##' Finde 5-Minuten-Intervall, in dem ein Event stattgefunden hat
##' Wenn kein Event stattgefunden hat, setze auf 0
##' @param site Standortname
##' @param image Motiv/Foto
##' @param events events-Datensatz
##' @param timeframes Beobachtungszeitraum-Datensatz
##' @return Events in 5-Minuten-Raster
get_event.in.raster <- function(site, image, events, timeframes){
  
  ev <- events[[as.character(image)]][events[[as.character(image)]]$Standortname == site, ]
  raster <- as.POSIXlt(seq(timeframes[timeframes$Standortname == site, ]$on, timeframes[timeframes$Standortname == site, ]$off, by = 5*60))
  
  ev$eventstart <- as.POSIXlt(ev$eventstart)
  ev$eventstart$min <- round.floor(ev$eventstart$min, 5)
  is.event <- raster %in% ev$eventstart
  
  
  ev.in.raster <- data.frame(time = raster, event = as.numeric(is.event), site = site)
  names(ev.in.raster)[names(ev.in.raster) == "event"] <- as.character(image)
  return(ev.in.raster)
}

sites <- levels(dt$Standortname)



## Gehe durch alle Fotos/Motive und erhalte Events in 5-Minuten-Raster als Liste
events.in.raster_list <- list()
for(mot in names(events)) {
  print(mot)
  events.in.raster_list[[mot]] <- ldply(sites, get_event.in.raster, image = mot, events = events, timeframes = timeframes, .progress = "text")
}
str(events.in.raster_list)
saveRDS(events.in.raster_list, "../EventsInRaster_list.rds")





## Erstelle Datensatz mit Events in 5-Minuten Raster
merge.all <- function(x, y) {
  merge(x, y, all=TRUE, by=c("time", "site"))
}
events.in.raster <- Reduce(merge.all, events.in.raster_list)
saveRDS(events.in.raster, "../EventsInRaster.rds")
