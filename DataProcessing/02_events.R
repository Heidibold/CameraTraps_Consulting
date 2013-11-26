###########################################
# Eventdaten erstellen
# - Kameras in Kamera 1 und 2 teilen
# - Kameradatensaetze in wide-format bringen
# - jede Minute des Beobachtungszeitraums in Datensatz einfuegen
# - event_Motiv Datensaetze erstellen mit Info eventstart, eventend, eventstartind, eventendind, eventlength
# in: Basis_Datensatz.rds
# out: Events_Info.rds
# by: Heidi
###########################################
# setwd("Z:/DropboxPortableAHK/Dropbox/Consulting/Daten")
# setwd("/home/heidi/Dropbox/Consulting/Daten/code/")
library(plyr)
# rm(list = ls())
dt <- readRDS("../Basis_Datensatz.rds")
## nur relevante Fotos auswaehlen
# paste(levels(dt$Foto), collapse = "', '")
dt <- dt[dt$Foto %in% c('Dachs', 'Fuchs', 'Hase', 'Hauskatze', 'Luchs', 'Marder', 'Mensch', 'MenschAuto', 'Rehwild', 'Rotwild', 'Wildschwein'), ]
dt$Foto <- factor(dt$Foto)


## was ist ein Doppelbild? 
## Gehe durch jeweils "erste" Kamera am Standort und checke, ob vor oder nach jedem Foto innerhalb von der gleichen Minute in Kamera 2 ein Foto gemacht wurde 
# paste(names(dt), collapse = "', '")
dt <- dt[ ,c( 'Standortname', 'Kameranummer', 'Zeit', 'Foto')]

kna <- which(is.na(dt$Kameranummer))
dt <- dt[-kna, ]
dt$kn <- dt$Kameranummer %% 2
dt$N <- 1
# ggplot(dt, aes(Kameranummer, Standortname)) + geom_point(aes(colour = kn))
K1 <- dt[dt$kn == 1, ]
K2 <- dt[dt$kn == 0, ]

motives <- levels(dt$Foto)
rm(dt)


library(reshape2)
K1wide <- dcast(K1, Standortname + Zeit ~ Foto, fun.aggregate = function(x){as.numeric(length(x) > 0)}, value.var="N")
K2wide <- dcast(K2, Standortname + Zeit ~ Foto, fun.aggregate = function(x){as.numeric(length(x) > 0)}, value.var="N")

# Kam <- K1wide[K1wide$Standortname == "5-Rindelberg", ]
# K1wide <- K1wide[K1wide$Standortname == "5-Rindelberg" | K1wide$Standortname == "6-Lusenparkplatz", ]

nProZeit <- function(Kam){
  timeframe <- data.frame(Zeit = seq(min(Kam$Zeit), max(Kam$Zeit), by = "min"), Standortname = unique(Kam$Standortname))
  datfullk <- merge(timeframe, Kam, by = c("Zeit", "Standortname"), all = TRUE)
  datfullk[is.na(datfullk)] <- 0
  datfullk <- datfullk[order(datfullk$Zeit), ]
  return(datfullk)
}

# running parallel
library(doParallel)
cl <- makeCluster(5)
registerDoParallel(cl)
datfullk1_list <- ddply(K1wide, .(Standortname), nProZeit, .progress = "text", .parallel=TRUE) 
datfullk2_list <- ddply(K2wide, .(Standortname), nProZeit, .progress = "text", .parallel=TRUE) 
rm(K1wide, K2wide)

get_eventbereich <- function(datfullk, Motiv = "Mensch") {
  s <- as.numeric(datfullk[ ,Motiv][1] == 1)
  e <- as.numeric(datfullk[ ,Motiv][nrow(datfullk)] == 1)
  dMensch <- c(s, diff(datfullk[ ,Motiv]))
  eventstartind <- which(dMensch == 1)
  eventendind <- which(dMensch == -1) - 1
  if(e == 1) eventendind <- c(eventendind, nrow(datfullk))
  eventstart <- datfullk$Zeit[eventstartind]
  eventend <- datfullk$Zeit[eventendind]
  res <- data.frame(event = seq_along(eventstart), eventstart, eventend, eventstartind, eventendind, eventlength = eventendind - eventstartind + 1)
  if(nrow(res) > 0 && max(res$eventlength) > 5) warning("maximal eventlength is more than 5 Minutes")
  return(res)
}

events <- list()
for(mot in motives) {
  print(mot)
  k1 <- ddply(datfullk1_list, .(Standortname), get_eventbereich, Motiv = mot, .progress = "text", .parallel=TRUE)
  k2 <- ddply(datfullk2_list, .(Standortname), get_eventbereich, Motiv = mot, .progress = "text", .parallel=TRUE)
  k1$Kamera <- 1
  k2$Kamera <- 2
  events[[mot]] <- rbind(k1, k2)
}
stopCluster(cl)
saveRDS(events, "../Events_Info1.rds")



# ###########################################
# # Eventdaten erstellen
# # - Kameras in Kamera 1 und 2 teilen
# # - Kameradatensaetze in wide-format bringen
# # - jede Minute des Beobachtungszeitraums in Datensatz einfuegen
# # - event_Motiv Datensaetze erstellen mit Info eventstart, eventend, eventstartind, eventendind, eventlength
# # in: Basis_Datensatz.rds
# # out: Events_Info.rds
# # by: Heidi
# ###########################################
# # setwd("Z:/DropboxPortableAHK/Dropbox/Consulting/Daten")
# # setwd("/home/heidi/Dropbox/Consulting/Daten")
# library(plyr)
# rm(list = ls())
# dt <- readRDS("Basis_Datensatz.rds")
# ## nur relevante Fotos auswaehlen
# # paste(levels(dt$Foto), collapse = "', '")
# dt <- dt[dt$Foto %in% c('Fuchs', 'Hase', 'Hauskatze', 'Luchs', 'Marder', 'Mensch', 'MenschAuto', 'Rehwild', 'Rotwild', 'Wildschwein'), ]
# dt$Foto <- factor(dt$Foto)
# 
# 
# ## was ist ein Doppelbild? 
# ## Gehe durch jeweils "erste" Kamera am Standort und checke, ob vor oder nach jedem Foto innerhalb von der gleichen Minute in Kamera 2 ein Foto gemacht wurde 
# # paste(names(dt), collapse = "', '")
# dt <- dt[ ,c( 'Standortname', 'Kameranummer', 'Zeit', 'Foto')]
# 
# kna <- which(is.na(dt$Kameranummer))
# dt <- dt[-kna, ]
# dt$kn <- dt$Kameranummer %% 2
# dt$N <- 1
# # ggplot(dt, aes(Kameranummer, Standortname)) + geom_point(aes(colour = kn))
# K1 <- dt[dt$kn == 1, ]
# K2 <- dt[dt$kn == 0, ]
# 
# motives <- levels(dt$Foto)
# rm(dt)
# 
# 
# library(reshape2)
# K1wide <- dcast(K1, Standortname + Zeit ~ Foto, fun.aggregate = function(x){as.numeric(length(x) > 0)}, value.var="N")
# K2wide <- dcast(K2, Standortname + Zeit ~ Foto, fun.aggregate = function(x){as.numeric(length(x) > 0)}, value.var="N")
# 
# # Kam <- K1wide[K1wide$Standortname == "5-Rindelberg", ]
# # K1wide <- K1wide[K1wide$Standortname == "5-Rindelberg" | K1wide$Standortname == "6-Lusenparkplatz", ]
# 
# nProZeit <- function(Kam){
#   timeframe <- data.frame(Zeit = seq(min(Kam$Zeit), max(Kam$Zeit), by = "min"), Standortname = unique(Kam$Standortname))
#   datfullk <- merge(timeframe, Kam, by = c("Zeit", "Standortname"), all = TRUE)
#   datfullk[is.na(datfullk)] <- 0
#   datfullk <- datfullk[order(datfullk$Zeit), ]
#   return(datfullk)
# }
# 
# # running parallel
# library(doParallel)
# cl <- makeCluster(5)
# registerDoParallel(cl)
# datfullk1_list <- dlply(K1wide, .(Standortname), nProZeit, .progress = "text", .parallel=TRUE) 
# datfullk2_list <- dlply(K2wide, .(Standortname), nProZeit, .progress = "text", .parallel=TRUE) 
# rm(K1wide, K2wide)
# 
# get_eventbereich <- function(datfullk, Motiv = "Mensch") {
#   s <- as.numeric(datfullk[ ,Motiv][1] == 1)
#   e <- as.numeric(datfullk[ ,Motiv][nrow(datfullk)] == 1)
#   dMensch <- c(s, diff(datfullk[ ,Motiv]))
#   eventstartind <- which(dMensch == 1)
#   eventendind <- which(dMensch == -1) - 1
#   if(e == 1) eventendind <- c(eventendind, nrow(datfullk))
#   eventstart <- datfullk$Zeit[eventstartind]
#   eventend <- datfullk$Zeit[eventendind]
#   res <- data.frame(event = seq_along(eventstart), eventstart, eventend, eventstartind, eventendind, eventlength = eventendind - eventstartind + 1)
#   if(nrow(res) > 0 && max(res$eventlength) > 5) warning("maximal eventlength is more than 5 Minutes")
#   return(res)
# }
# 
# events <- list()
# for(mot in motives) {
#   print(mot)
#   events[[mot]][["k1"]] <- llply(datfullk1_list, get_eventbereich, Motiv = mot, .progress = "text", .parallel=TRUE)
#   events[[mot]][["k2"]] <- llply(datfullk2_list, get_eventbereich, Motiv = mot, .progress = "text", .parallel=TRUE)
# }
# stopCluster(cl)
# saveRDS(events, "Events_Info.rds")
# 
# 
