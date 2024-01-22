#############
# Aufgabe 4 #
#############

library(ggplot2)
library(plotly)
library(dplyr)

daten <- read.csv(
  file = "C:\\Users\\sebas\\OneDrive - uni-bielefeld.de\\Desktop\\Computergestützte Methoden\\Abgabe 2\\Capital_bikeshare_data_2022_with_NAs.csv", 
  header = TRUE, 
  sep = ",",
  dec = "."
)

# Filtern der Daten.
stationDaten <- subset(daten, station == "7th & K St NW")

# Identifizieren von Anomalien - Negative Werte in Windgeschwindigkeit auf NA setzen.
stationDaten$wind_speed[stationDaten$wind_speed < 0] <- NA

# Imputation der NAs - Entfernen von Zeilen, in denen "count" NA ist.
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



########################
# Aufgabe 4.1 Grafik 1 #
########################

# Aufgabe 4.1 Grafik 1
Aufgabe_4.1_Grafik_1 <- ggplot(datenOhneNAsInCount, aes(x = mean_temperature, y = count)) +
  geom_point(aes(color = mean_temperature)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  labs(title = "Zusammenhang zwischen Temperatur und Anzahl ausgeliehener Fahrräder:",
       x = "Temperatur in Grad Fahrenheit",
       y = "Anzahl ausgeliehener Fahrräder") +
  scale_color_gradient(name = "Temperaturverlauf:", low = "blue", high = "red") +
  theme_minimal()

plot(Aufgabe_4.1_Grafik_1)
ggsave(plot = Aufgabe_4.1_Grafik_1, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.1_Grafik_1.pdf", width = 8, height = 5)

# Aufgabe 4.1 Grafik 2
Aufgabe_4.1_Grafik_2 <- ggplot(datenOhneNAsInCount, aes(x = precipitation, y = count)) +
  geom_point(aes(size = precipitation, color = precipitation)) +
  labs(title = "Zusammenhang zwischen Niederschlagsmenge und Anzahl ausgeliehener Fahrräder:",
       x = "Niederschlagsmenge",
       y = "Anzahl ausgeliehener Fahrräder") +
  scale_size(range = c(1, 5)) +
  scale_color_gradient(name = "Niederschlagsmenge:", low = "lightblue", high = "darkblue") +
  theme_minimal() +
  guides(size = "none")

plot(Aufgabe_4.1_Grafik_2)
ggsave(plot = Aufgabe_4.1_Grafik_2, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.1_Grafik_2.pdf", width = 8, height = 8)

# Aufgabe 4.1 Grafik 3
Aufgabe_4.1_Grafik_3 <- ggplot(datenOhneNAsInCount, aes(x = wind_speed, y = count)) +
  geom_point(aes(color = wind_speed)) +
  labs(title = "Zusammenhang zwischen Windgeschwindigkeit und Anzahl ausgeliehener Fahrräder:",
       x = "Windgeschwindigkeit",
       y = "Anzahl ausgeliehener Fahrräder") +
  scale_color_gradient(name = "Windgeschwindigkeit:", low = "lightblue", high = "red") +
  theme_minimal()

plot(Aufgabe_4.1_Grafik_3)
ggsave(plot = Aufgabe_4.1_Grafik_3, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.1_Grafik_3.pdf", width = 8, height = 5)

# Date-Format umwandeln.
datenOhneNAsInCount$date <- as.Date(datenOhneNAsInCount$date)
# Umwandeln des Datums in Monate als Faktor.
datenOhneNAsInCount$month_year <- format(datenOhneNAsInCount$date, "%Y-%m")
# Daten in Spalte Date sortieren.
datenOhneNAsInCount <- datenOhneNAsInCount %>% arrange(date)

# Aufgabe 4.1 Grafik 4
Aufgabe_4.1_Grafik_4 <- ggplot(datenOhneNAsInCount, aes(x = date, y = count, group = 1)) +
  geom_line(size = 0.5) +
  geom_point(size = 1, alpha = 1) + 
  labs(title = "Zeitlicher Verlauf der Anzahl ausgeliehener Fahrräder:",
       x = "Datum",
       y = "Anzahl ausgeliehener Fahrräder") +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1))

plot(Aufgabe_4.1_Grafik_4)
ggsave(plot = Aufgabe_4.1_Grafik_4, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.1_Grafik_4.pdf", width = 12, height = 6)



###############
# Aufgabe 4.2 #
###############

# Base-Plot:
base_plot <- ggplot(data = datenOhneNAsInCount, aes(x = mean_temperature, y = count)) +
  labs(x = "Temperatur in Grad Fahrenheit", 
       y = "Anzahl ausgeliehener Fahrräder") +
  theme_minimal()

# Aufgabe_4.2_Grafik_1 - Grafik für Regentage:
Aufgabe_4.2_Grafik_1 <- base_plot +
  geom_point(data = datenOhneNAsInCount[datenOhneNAsInCount$precipitation > 0, ], aes(color = precipitation > 0), alpha = 0.5, size = 3) +
  geom_smooth(data = datenOhneNAsInCount[datenOhneNAsInCount$precipitation > 0, ], aes(color = precipitation > 0), method = "lm", se = FALSE) +
  labs(title = "Anzahl ausgeliehener Fahrräder an Regentagen in Abhängigkeit von der Temperatur:") +
  scale_color_manual(name = "", values = c("blue"), labels = c("Regen"))

# Aufgabe_4.2_Grafik_2 - Grafik für Nichtregentage:
Aufgabe_4.2_Grafik_2 <- base_plot +
  geom_point(data = datenOhneNAsInCount[datenOhneNAsInCount$precipitation == 0, ], aes(color = precipitation > 0), alpha = 0.5, size = 3) +
  geom_smooth(data = datenOhneNAsInCount[datenOhneNAsInCount$precipitation == 0, ], aes(color = precipitation > 0), method = "lm", se = FALSE) +
  labs(title = "Anzahl ausgeliehener Fahrräder an Nichtregentagen in Abhängigkeit von der Temperatur:") +
  scale_color_manual(name = "", values = c("red"), labels = c("Kein Regen"))

# Aufgabe_4.2_Grafik_3_Zusatz - Kombinierte Grafik:
Aufgabe_4.2_Grafik_3_Zusatz <- base_plot +
  geom_point(aes(color = precipitation > 0), alpha = 0.5, size = 3) +
  geom_smooth(aes(color = precipitation > 0), method = "lm", se = FALSE) +
  scale_color_manual(name = "", values = c("red", "blue"), labels = c("Kein Regen", "Regen")) +
  facet_wrap(~ precipitation > 0, labeller = as_labeller(c(`FALSE` = "Nichtregentage", `TRUE` = "Regentage")))

plot(Aufgabe_4.2_Grafik_1)
plot(Aufgabe_4.2_Grafik_2)
plot(Aufgabe_4.2_Grafik_3_Zusatz)
ggsave(plot = Aufgabe_4.2_Grafik_1, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.2_Grafik_1.pdf", width = 10, height = 8)
ggsave(plot = Aufgabe_4.2_Grafik_2, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.2_Grafik_2.pdf", width = 10, height = 8)
ggsave(plot = Aufgabe_4.2_Grafik_3_Zusatz, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.2_Grafik_3_Zusatz.pdf", width = 14, height = 8)



###############
# Aufgabe 4.3 #
###############

# Aufgabe 4.3 Grafik 1
# Zeigt an, wie oft eine bestimmte Anzahl an Fahrrädern ausgeliehen wurde.
Aufgabe_4.3_Grafik_1 <- ggplot(datenOhneNAsInCount, aes(x = count)) +
  geom_histogram(binwidth = 6, fill = "gray", color = "black") +
  labs(title = "Verteilung der Anzahl ausgeliehener Fahrräder:",
       x = "Anzahl ausgeliehener Fahrräder",
       y = "Häufigkeit") +
  theme_minimal()

plot(Aufgabe_4.3_Grafik_1)
ggsave(plot = Aufgabe_4.3_Grafik_1, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.3_Grafik_1.pdf", width = 12, height = 6)


# Aufgabe 4.3 Grafik 2
# Zeigt an, welche Temperaturen am häufigsten vorkommen und wie die Temperaturen über einen bestimmten Bereich verteilt sind.
Aufgabe_4.3_Grafik_2 <-ggplot(datenOhneNAsInCount, aes(x = mean_temperature)) +
  geom_density(fill = "gray") +
  labs(title = "Verteilung der Temperatur:",
       x = "Temperatur in Grad Fahrenheit",
       y = "Dichte") +
  theme_minimal()

plot(Aufgabe_4.3_Grafik_2)
ggsave(plot = Aufgabe_4.3_Grafik_2, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.3_Grafik_2.pdf", width = 12, height = 6)


# Daten filtern, um nur Tage mit Regen zu berücksichtigen.
regenTage <- datenOhneNAsInCount %>% filter(precipitation > 0)

# Aufgabe 4.3 Grafik 3
# Zeigt an, an wie vielen Tagen es zu welcher Niederschlagsmenge kam.
Aufgabe_4.3_Grafik_3 <- ggplot(regenTage, aes(x = precipitation)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black", boundary = 0.01) +
  labs(title = "Verteilung der Niederschlagsmenge an Regentagen:",
       x = "Niederschlagsmenge",
       y = "Häufigkeit") +
  theme_minimal()

plot(Aufgabe_4.3_Grafik_3)
ggsave(plot = Aufgabe_4.3_Grafik_3, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.3_Grafik_3.pdf", width = 12, height = 6)


# Aufgabe 4.3 Grafik 4
# Zeigt an, welche Windgeschwindigkeiten am häufigsten auftreten und wie breit die Verteilung der Windgeschwindigkeiten ist.
Aufgabe_4.3_Grafik_4 <- ggplot(datenOhneNAsInCount, aes(x = wind_speed)) +
  geom_density(fill = "gray") +
  labs(title = "Verteilung der Windgeschwindigkeit:",
       x = "Windgeschwindigkeit",
       y = "Dichte") +
  theme_minimal()

plot(Aufgabe_4.3_Grafik_4)
ggsave(plot = Aufgabe_4.3_Grafik_4, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.3_Grafik_4.pdf", width = 12, height = 6)



###############
# Aufgabe 4.4 #
###############

# Aufteilen der Daten nach Jahreszeit:
# Winter: 2022-01-01 bis 2022-03-19
# Frühling: 2022-03-20 bis 2022-06-20
# Sommer: 2022-06-21 bis 2022-09-22
# Herbst: 2022-09-23 bis 2022-11-30

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
Aufgabe_4.4_Grafik_1 <- ggplot(datenOhneNAsInCount, aes(x = count, fill = jahreszeit)) +
  geom_density(alpha = 0.7) +
  labs(title = "Verteilung der Anzahl ausgeliehener Fahrräder nach Jahreszeit:",
       x = "Anzahl ausgeliehener Fahrräder",
       y = "Dichte") +
  scale_fill_manual(values = c("Winter" = "blue", "Frühling" = "green", "Sommer" = "yellow", "Herbst" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

plot(Aufgabe_4.4_Grafik_1)
ggsave(plot = Aufgabe_4.4_Grafik_1, file = "C://Users//sebas//OneDrive - uni-bielefeld.de//Desktop//Computergestützte Methoden//Aufgabe_4.4_Grafik_1.pdf", width = 12, height = 8)



###############
# Aufgabe 4.5 #
###############

# Aufgabe 4.5 Grafik 1 - Erstellen des 3D-Scatterplots:
Aufgabe_4.5_Grafik_1 <- plot_ly(datenOhneNAsInCount, x = ~mean_temperature, y = ~wind_speed, z = ~count, 
                                type = 'scatter3d', mode = 'markers',
                                marker = list(size = 3, color = ~count, colorscale = 'Viridis', opacity = 0.8),
                                text = ~paste("Temperatur:", mean_temperature, "Windgeschwindigkeit:", wind_speed, 
                                              "Anzahl Fahrräder:", count),
                                hoverinfo = 'text') %>%
                        layout(scene = list(xaxis = list(title = 'Temperatur'),
                        yaxis = list(title = 'Windgeschwindigkeit'),
                        zaxis = list(title = 'Anzahl ausgeliehener Fahrräder')))

Aufgabe_4.5_Grafik_1





















