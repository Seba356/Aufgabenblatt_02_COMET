library(ggplot2)

daten <- read.csv(
  file = "C:\\Users\\sebas\\OneDrive - uni-bielefeld.de\\Desktop\\Computergestützte Methoden\\Abgabe 2\\Capital_bikeshare_data_2022_with_NAs.csv", 
  header = TRUE, 
  sep = ",",
  dec = "."
)

# Filtern der Daten.
stationDaten <- subset(daten, station == "7th & K St NW")

# Identifizieren von Anomalien:
# Negative Werte in Windgeschwindigkeit auf NA setzen.
stationDaten$wind_speed[stationDaten$wind_speed < 0] <- NA

# Imputation der NAs:
# Entfernen von Zeilen, in denen "count" NA ist.
datenOhneNAsInCount <- stationDaten[!is.na(stationDaten$count), ]

# Identifizieren von Spalten, die mindestens einen NA-Wert enthalten.
spaltenMitNAs <- sapply(datenOhneNAsInCount, anyNA)

# Ausgabe der Namen der Spalten mit NA-Werten.
spaltenNamenMitNAs <- names(datenOhneNAsInCount)[spaltenMitNAs]
print(spaltenNamenMitNAs)
# [1] "wind_speed", "mean_temperature"

# 1. Identifizieren von Tagen mit fehlenden Wetterdaten (wind_speed).
tageMitFehlendemWindSpeed <- unique(datenOhneNAsInCount$date[is.na(datenOhneNAsInCount$wind_speed)])
print(tageMitFehlendemWindSpeed)
# "2022-06-21" "2022-06-26" "2022-08-08"

# 2. Identifizieren von Tagen mit fehlenden Wetterdaten (mean_temperature).
tageMitFehlenderTemperature <- unique(datenOhneNAsInCount$date[is.na(datenOhneNAsInCount$mean_temperature)])
print(tageMitFehlenderTemperature)
# "2022-01-12" "2022-02-13" "2022-11-13"

# Durchlaufen der Tage und Ersetzen der fehlenden Daten für die Spalte "wind_speed".
for (tag in tageMitFehlendemWindSpeed) {
  if (is.na(datenOhneNAsInCount$wind_speed[datenOhneNAsInCount$date == tag])) {
    # Suche nach einem "nicht-NA-Wert" für "wind_speed" an diesem Tag im vollständigen Datensatz.
    ersatzWindSpeed <- daten$wind_speed[daten$date == tag & !is.na(daten$wind_speed)][1]
    datenOhneNAsInCount$wind_speed[datenOhneNAsInCount$date == tag] <- ersatzWindSpeed
  }
}

# Durchlaufen der Tage und Ersetzen der fehlenden Daten für die Spalte "mean_temperature".
for (tag in tageMitFehlenderTemperature) {
  if (is.na(datenOhneNAsInCount$mean_temperature[datenOhneNAsInCount$date == tag])) {
    # Suche nach einem "nicht-NA-Wert" für "mean_temperature" an diesem Tag im vollständigen Datensatz.
    ersatzTemperature <- daten$mean_temperature[daten$date == tag & !is.na(daten$mean_temperature)][1]
    datenOhneNAsInCount$mean_temperature[datenOhneNAsInCount$date == tag] <- ersatzTemperature
  }
}

# Überprüfen auf weiterhin vorhandene NA-Werte in "wind_speed".
anzahlNAsWindSpeed <- sum(is.na(datenOhneNAsInCount$wind_speed))
print(paste("Anzahl fehlender Werte in wind_speed:", anzahlNAsWindSpeed))
# "Anzahl fehlender Werte in wind_speed: 0"

# Überprüfen auf weiterhin vorhandene NA-Werte in "mean_temperature".
anzahlNAsMeanTemperature <- sum(is.na(datenOhneNAsInCount$mean_temperature))
print(paste("Anzahl fehlender Werte in mean_temperature:", anzahlNAsMeanTemperature))
# "Anzahl fehlender Werte in mean_temperature: 0"



