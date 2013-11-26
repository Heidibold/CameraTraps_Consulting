###########################################
# Funktionen, die in R-Code 03_eventsBothCameras 
# helfen 
# by: Heidi
###########################################


##' Ist Zeitpunkt tp im Intervall zwischen start und end
##' @param tp Zeitpunkt
##' @param start Vektor an Startzeitpunkten
##' @param end Vektor an Endzeitpunkten
##' @return Nummer des Intervalls, in dem der Zeitpunkt liegt. Wenn tp in 
##' keinem Intervall liegt NA
tpointInIterval <- function(tp, start, end){
  inint <- tp >= start & tp <= end
  ifelse(sum(inint)>0, which(inint), NA)
}

##' Mache aus mehrern ueberlappenden Events eins
##' @param x data.frame mit eventstart und -end Info fuer beide Kameras der ueberlappenden Events
##' @return data.frame mit einer Zeile
aggregate_events <- function(x){
  res <- x[1,]
  starts <- c(x$eventstart.k1, x$eventstart.k2)
  ends <- c(x$eventend.k1, x$eventend.k2)
  res$eventstart <- min(starts)
  res$eventend <- max(ends)
  if(length(unique(x$event.k1)) != 1) res$event.k1 <- paste(x$event.k1, collapse = "/")
  if(length(unique(x$event.k2)) != 1) res$event.k2 <- paste(x$event.k2, collapse = "/")
  return(res)
}



##' Nimm Datensatz aus (eventuell zum Teil) ueberlappenden Events und fasse alle ueberlappenden zusammen
##' @param s startzeitpunkt-Datensatz
##' @param e endzeitpunkt-Datensatz
##' @return Zusammengefassten Datensatz
get_merged.events <- function(s, e){     
  all <- merge(s, e, all = TRUE)[ ,c("Standortname.k1", "event.k1", "eventstart.k1", "eventend.k1", "event.k2", "eventstart.k2", "eventend.k2")]
  
  ## finde ueberlappende Events
  temp <- duplicated(all$event.k1) + duplicated(all$event.k2)
  dup.end <- as.numeric(diff(temp) == -1)
  dup <- pmax(temp, c(temp[-1], 0))
  ## erstelle Hilfsvariablen, um den Datensatz fuer aggregate_events passen zu splitten
  all$dupseq <- cumsum((dup == 0) + c(0,dup.end)) * dup 
  all$nondupseq <- cumsum(c(1, dup[-length(dup)])) * (dup == 0) * seq_along(dup)    
  

  ## checke, ob es ueberlappende Events gibt
  if(sum(dup) == 0) { ## nein
    all.ready <- all
    all.ready$eventstart <- pmin(all.ready$eventstart.k1, all.ready$eventstart.k2)
    all.ready$eventend <- pmax(all.ready$eventend.k1, all.ready$eventend.k2)
  }else{ ## ja
    all.ready <- ddply(all, .variables = .(dupseq, nondupseq), .fun = aggregate_events)
  }
  
  all.ready$eventlength <- difftime(all.ready$eventend, all.ready$eventstart, units = "mins") + 1
  all.ready <- all.ready[ ,c("Standortname.k1", "event.k1", "event.k2", "eventstart", "eventend", "eventlength")]
  names(all.ready)[1] <- "Standortname"
  return(all.ready)
}



##' Nimm Start und Endzeitpunkte, rbinde sie und erstelle wichtige Variablen
##' @param s startzeitpunkt-Datensatz
##' @param e endzeitpunkt-Datensatz
##' @return Zusammengefassten Datensatz
rbind.se <- function(s, e){
  res <- rbind(s, e)
  res$eventstart <- pmin(res$eventstart.k1, res$eventstart.k2)
  res$eventend <- pmax(res$eventend.k1, res$eventend.k2)
  res$eventlength <- difftime(res$eventend, res$eventstart, units = "mins") + 1
  res <- res[ ,c("Standortname.k1", "event.k1", "event.k2", "eventstart", "eventend", "eventlength")]
  names(res)[1] <- "Standortname"
  return(res)
}


##' Finde ueberlappende Events
##' @param k1 Datensatz von Kamera 1
##' @param k2 Datensatz von Kamera 2
##' @return Zusammengefassten Datensatz
get_overlaps <- function(k1, k2) {
  names(k1) <- paste(names(k1), "k1", sep = ".")
  names(k2) <- paste(names(k2), "k2", sep = ".")
  
  ## Kameras koennen bis zu 2 Minuten falsch eingestellt sein
  start1 <- k1$eventstart.k1 - 60
  start2 <- k2$eventstart.k2 - 60
  end1 <- k1$eventend.k1 + 60
  end2 <- k2$eventend.k2 + 60
  
  s2in1 <- sapply(start2, tpointInIterval, start = start1, end = end1)
  e2in1 <- sapply(end2, tpointInIterval, start = start1, end = end1) 
#   if(class(s2in1) == "list" | class(e2in1) == "list") stop("More than two overlapping events. Please ask Heidi to change the function.")
  
  
  
  if(length(na.omit(s2in1)) > 0) { 
    s <- cbind(k1[na.omit(s2in1), ], k2[which(!is.na(s2in1)),])
  }else{
    s <- data.frame()
  }
  if(length(na.omit(e2in1)) > 0) {
    e <- cbind(k1[na.omit(e2in1), ], k2[which(!is.na(e2in1)),])
  }else{
    e <- data.frame()
  }
  
  spasst <- nrow(s) > 0
  epasst <- nrow(e) > 0
  
  ## je nach dimension von s und e, merge die beiden
  res <- switch(paste("r", sum(spasst, epasst), sep = ""),
                r0 = data.frame(),
                r1 = rbind.se(s, e),
                r2 = get_merged.events(s, e)
  )
  
  return(res)
}




###############################################


##' Nimm ueberlappende events und alle events fuer einen bestimmten Standort,
##' finde welche der events nicht in overlaps enthalten ist und bastle einen
##' Datensatz aus allen Events
##' @param sites Vektor mit Standort(site) und Motiv(image) Information
##' @param overlaps ueberlappende Events
##' @param ev alle Events
get_allevents <- function(sites, overlaps, ev){
  site <- sites[["site"]]
  mot <- sites[["image"]]
  
  ## overlaps
  a <- overlaps[[as.character(mot)]]
  x <- a[a$Standortname == site, ]
  
  ## all
  b <- ev[[as.character(mot)]]
  y <- b[b$Standortname == site, ]
  
  yk1 <- y[y$Kamera == 1, ]
  yk2 <- y[y$Kamera == 2, ]
  event.k1 <- yk1$event
  event.k2 <- yk2$event
  
  ## finde Events, die noch nicht in overlaps enthalten sind
  coln <- c('Standortname', 'eventstart', 'eventend', 'eventlength', 'Kamera')
  rest1 <- yk1[!(event.k1 %in% unlist(strsplit(as.character(x$event.k1), "/"))), coln]
  rest2 <- yk2[!(event.k2 %in% unlist(strsplit(as.character(x$event.k2), "/"))), coln]
  
  if(nrow(x) > 0) {
    x$Kamera <- "both"
    bothk <- x[ ,coln]
  }else{
    bothk <- data.frame()
  }
  
  res <- rbind(rest1, rest2, bothk)
  return(res)
}
