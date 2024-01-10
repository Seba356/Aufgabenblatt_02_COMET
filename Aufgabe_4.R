library(ggplot2)
library(plotly)

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

# Aufgabe 4.1 Grafik 1
ggplot(datenOhneNAsInCount, aes(x = mean_temperature, y = count)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Temperatur und Anzahl ausgeliehener Fahrräder:",
       x = "Temperatur in Grad Fahrenheit",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Aufgabe 4.1 Grafik 2
  ggplot(datenOhneNAsInCount, aes(x = precipitation, y = count)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Niederschlagsmenge und Anzahl ausgeliehener Fahrräder",
       x = "Niederschlagsmenge",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Aufgabe 4.1 Grafik 3
ggplot(datenOhneNAsInCount, aes(x = wind_speed, y = count)) +
  geom_point() +
  labs(title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder:",
       x = "Windgeschwindigkeit",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Aufgabe 4.1 Grafik 4

# Umwandlung der "date" Spalte in den Datentyp <Date>.
datenOhneNAsInCount$date <- as.Date(datenOhneNAsInCount$date, format = "%Y-%m-%d")

ggplot(datenOhneNAsInCount, aes(x = date, y = count)) +
  geom_line() +
  labs(title = "Zeitlicher Verlauf der Anzahl ausgeliehener Fahrräder:",
       x = "Datum",
       y = "Anzahl ausgeliehener Fahrräder")+
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_minimal()

# Aufgabe 4.2 Grafik 1

# Filtern des Datensatz nach Regentagen und "Nichtregentagen".
regenTage <- datenOhneNAsInCount[datenOhneNAsInCount$precipitation > 0,]
nichtRegenTage <- datenOhneNAsInCount[datenOhneNAsInCount$precipitation == 0,]

ggplot(regenTage, aes(x = mean_temperature, y = count)) +
  geom_point() +
  labs(title = "Anzahl ausgeliehener Fahrräder an Regentagen in Abhängigkeit von der Temperatur:",
       x = "Temperatur in Grad Fahrenheit",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Aufgabe 4.2 Grafik 2
ggplot(nichtRegenTage, aes(x = mean_temperature, y = count)) +
  geom_point() +
  labs(title = "Anzahl ausgeliehener Fahrräder an Nichtregentagen in Abhängigkeit von der Temperatur",
       x = "Temperatur in Grad Fahrenheit",
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Aufgabe 4.3 Grafik 1
# Es zeigt an, wie oft eine bestimmte Anzahl an Fahrrädern ausgeliehen wurde.
ggplot(datenOhneNAsInCount, aes(x = count)) +
  geom_histogram(binwidth = 6, fill = "gray", color = "black") +
  labs(title = "Verteilung der Anzahl ausgeliehener Fahrräder:",
       x = "Anzahl ausgeliehener Fahrräder",
       y = "Häufigkeit") +
  theme_minimal()

# Aufgabe 4.3 Grafik 2
# Es zeigt an, welche Temperaturen am häufigsten vorkommen und wie die Temperaturen über einen bestimmten Bereich verteilt sind.
ggplot(datenOhneNAsInCount, aes(x = mean_temperature)) +
  geom_density(fill = "gray") +
  labs(title = "Verteilung der Temperatur:",
       x = "Temperatur in Grad Fahrenheit",
       y = "Dichte") +
  theme_minimal()

# Aufgabe 4.3 Grafik 3
# Es zeigt an, an wie vielen Tagen es zu welcher Niederschlagsmenge kam.
ggplot(regenTage, aes(x = precipitation)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  labs(title = "Verteilung der Niederschlagsmenge:",
       x = "Niederschlagsmenge",
       y = "Häufigkeit") +
  theme_minimal()

# Aufgabe 4.3 Grafik 4
# Es zeigt an, welche Windgeschwindigkeiten am häufigsten auftreten und wie breit die Verteilung der Windgeschwindigkeiten ist.
ggplot(datenOhneNAsInCount, aes(x = wind_speed)) +
  geom_density(fill = "gray") +
  labs(title = "Verteilung der Windgeschwindigkeit:",
       x = "Windgeschwindigkeit",
       y = "Dichte") +
  theme_minimal()

# Aufgabe 4.4 
# Aufteilen der Daten nach Jahreszeit.
#
# Winter: 2022-01-01 bis 2022-03-19
# Frühling: 2022-03-20 bis 2022-06-20
# Sommer: 2022-06-21 bis 2022-09-22
# Herbst: 2022-09-23 bis 2022-11-30
#
# Sortieren des Datensatzes nach dem Datum.
datenOhneNAsInCount <- datenOhneNAsInCount[order(as.Date(datenOhneNAsInCount$date)), ]
datenOhneNAsInCount$date <- as.Date(datenOhneNAsInCount$date, format = "%y-%m-%d")

# Zuweisen der Jahreszeiten.
jahreszeit <- function(datum) {
  if (datum >= as.Date("2022-01-01") & datum < as.Date("2022-03-20")) {
    return("Winter")
  } else if (datum >= as.Date("2022-03-20") & datum < as.Date("2022-06-21")) {
    return("Frühling")
  } else if (datum >= as.Date("2022-06-21") & datum < as.Date("2022-09-23")) {
    return("Sommer")
  } else if (datum >= as.Date("2022-09-23") & datum <= as.Date("2022-11-30")) {
    return("Herbst")
  } else {
    return(NA)
  }
}

# Anwenden der Funktion auf jede Zeile des Datensatzes.
datenOhneNAsInCount$jahreszeit <- sapply(datenOhneNAsInCount$date, jahreszeit)

# Aufgabe 4.4 Grafik 1
ggplot(datenOhneNAsInCount, aes(x = count, fill = jahreszeit)) +
  geom_density(alpha = 0.7) +
  labs(title = "Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeit:",
       x = "Anzahl ausgeliehener Fahrräder",
       y = "Dichte") +
  scale_fill_manual(values = c("Winter" = "blue", "Frühling" = "green", "Sommer" = "yellow", "Herbst" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

# Aufgabe 4.5 Grafik 1
#
# Erstellen des 3D-Scatterplots:
scatterplot <- plot_ly(datenOhneNAsInCount, x = ~mean_temperature, y = ~wind_speed, z = ~count, type = 'scatter3d', mode = 'markers',
               marker = list(size = 3, color = ~count, colorscale = 'Viridis', opacity = 0.8))

# Einfügen von Achsentitel:
scatterplot <- fig %>% layout(scene = list(xaxis = list(title = 'Temperatur'),
                                   yaxis = list(title = 'Windgeschwindigkeit'),
                                   zaxis = list(title = 'Anzahl ausgeliehener Fahrräder')))
scatterplot












