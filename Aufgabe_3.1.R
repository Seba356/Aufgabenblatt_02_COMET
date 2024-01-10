# Funktion zur Simulation des Wichtelns unter n Personen.
# 
# Diese Funktion simuliert das Wichteln, bei dem jede der n Personen zufällig ein Geschenk auswählt und berechnet die Wahrscheinlichkeit,
# dass mindestens eine Person das eigene Geschenk zurückbekommt.
#
# Argumente:
#   Keine. Die Anzahl der Personen (n) ist fest auf 10 gesetzt.
#
# Rückgabewert:
#   Die Wahrscheinlichkeit, dass unter 10 Personen mindestens eine 
#   ihr eigenes Geschenk zurückerhält.
#
# Setze einen Seed, um reproduzierbare Ergebnisse zu erhalten.
set.seed(10)

# Anzahl der Durchläufe für die Simulation.
n <- 1000

# Zähler für die Anzahl der Ereignisse, bei denen mindestens eine Person ihr eigenes Geschenk zieht.
ereignisZaehler <- 0

# Führe die Simulation n-mal durch.
for (i in 1:n) {
  # Erzeuge eine zufällige Permutation der Zahlen von 1 bis 10, die die Geschenkverteilung darstellt.
  x <- sample(1:10)
  # Überprüfe, ob mindestens eine Zahl an ihrer ursprünglichen Position ist.
  if (any(x == 1:10)) {
    ereignisZaehler <- ereignisZaehler + 1
  }
}

# Berechne die Wahrscheinlichkeit, dass mindestens eine Person ihr eigenes Geschenk zieht.
mu <- ereignisZaehler / n
mu