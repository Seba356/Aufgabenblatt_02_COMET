# Funktion zur Berechnung der Wahrscheinlichkeit, dass unter n Personen mindestens k ihr eigenes Geschenk zurückbekommen.
#
# Argumente:
#   n (numeric): Die Anzahl der Personen.
#   k (numeric): Die Mindestanzahl von Personen, die ihr eigenes Geschenk zurückbekommen sollen.
#   iterationen (numeric): Die Anzahl der Iterationen für die Simulation. Standardmäßig auf 1000 gesetzt.
#
# Rückgabewert:
#   Die Wahrscheinlichkeit, dass unter n Personen mindestens k ihr eigenes Geschenk zurückbekommen.
#
# Beispiel:
#   wichtelUnglueck(1000, 2) # Gibt die Wahrscheinlichkeit zurück, dass unter 1000 Personen mindestens zwei ihr eigenes Geschenk zurückbekommt.
#
wichtelUnglueck <- function(n, k, iterationen = 1000) {
  # Zähler für die Anzahl der Ereignisse, bei denen mindestens k Personen ihr eigenes Geschenk ziehen.
  ereignisZaehler <- 0
  
  # Führe die Simulation "Iterationen"-mal durch.
  for (i in 1:iterationen) {
    # Erzeuge eine zufällige Permutation der Zahlen von 1 bis n, die hier die Geschenkverteilung darstellt.
    x <- sample(1:n)
    
    # Überprüfe, ob mindestens k Zahlen an ihrer ursprünglichen Position sind.
    if (sum(x == 1:n) >= k) {
      ereignisZaehler <- ereignisZaehler + 1
    }
  }
  
  # Berechne die Wahrscheinlichkeit des Ereignisses.
  mu <- ereignisZaehler / iterationen
  return(mu)
}


