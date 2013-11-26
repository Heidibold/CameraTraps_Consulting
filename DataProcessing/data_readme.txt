01_preprocessing.R

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


02_events.R

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


03_eventsBothCameras.R
03_eventsBosthCameras_HelperFunctions.R

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


04_eventsInRaster.R

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


05_addTimeAndWeather.R

###############################################
# Fuege Zeit und Wetterinfo zu Datensatz hinzu
# in: EventsInRaster.rds, Wetterdaten.xls
# out: EventsInRaster_TimeWeather.rds
# by: Heidi
###############################################


06_PathCategory.R

###############################################
# Fuege Wegkategorie hinzu
# in: EventsInRaster_TimeWeather.rds, Wegkategorien.csv
# out: EventsInRaster_TimeWeatherPath.rds
# by: Heidi
###############################################









