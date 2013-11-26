###########################################
# Datensatz für beide Kameras gemeinsam erstellen
# Ueberlappende Events als solch behandeln
# - finde ueberlappende events
# - erstelle datensatz der ueberlappenden events (overlaps)
# - erstelle datensatz aller events
# in: Events_Info.rds
# out: Events_Info2.rds
# by: Heidi
###########################################
# setwd("Z:/Dropbox/Consulting/Daten/code/")
# setwd("/home/heidi/Dropbox/Consulting/Daten/code")
library(ggplot2)
library(plyr)
ev <- readRDS("../Events_Info1.rds")
source('03_eventsBothCameras_HelperFunctions.R')

# str(ev,1)
# x <- ev$Fuchs
# k1 <- x[x$Kamera == 1 & x$Standortname == "10-Racheldiensthuette", ]
# k2 <- x[x$Kamera == 2 & x$Standortname == "10-Racheldiensthuette", ]



### get overlapping events in camera 1 and 2
overlaps <- list()
for(mot in names(ev)) {
  print(mot)
  
  overlaps[[mot]] <- ddply(ev[[mot]], .(Standortname), function(x) {
    k1 <- x[x$Kamera == 1, ]
    k2 <- x[x$Kamera == 2, ]
    get_overlaps(k1, k2)
  }, .progress = "text")
  
}
str(overlaps,1)
summary(overlaps[[1]])

ldply(overlaps, function(x){sum(table(x$Standortname, x$event.k1) > 1)})
ldply(overlaps, function(x){sum(table(x$Standortname, x$event.k2) > 1)})

saveRDS(overlaps, "../Events_overlaps.rds")





### get all events (overlapping and non overlapping)
# ev <- readRDS("../Events_Info1.rds")
# overlaps <- readRDS("../Events_overlaps.rds")
# site <- "3-WG Riedlhaeng"

ev2 <- list()
for(mot in names(ev)) {
  print(mot)
  sites <- data.frame(site = levels(ev[[1]]$Standortname), image = mot)
 ev2[[mot]] <- ddply(sites, .(site, image), get_allevents, 
                     .progress = "text", overlaps = overlaps, ev = ev)
}

saveRDS(ev2, "../Events_Info2.rds")

## Table of event-length
eventtable1 <- ldply(ev, function(x) as.data.frame(table(x$eventlength)))
eventtable2 <- ldply(ev2, function(x) as.data.frame(table(x$eventlength)))
names(eventtable1) <- c("image", "eventlength", "frequency")
names(eventtable2) <- c("image", "eventlength", "frequency")
saveRDS(eventtable2, "../event_table.rds") 















