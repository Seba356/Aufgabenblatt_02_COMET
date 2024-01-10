library(testthat)

wichtelUnglueck <- function(n, k, iterationen = 1000) {
  # Überprüfen, ob "n" und "k" 0 sind.
  if (n == 0 && k == 0) {
    return(0)
  }
  
  # Überprüfen, ob "n" oder "k" negativ sind.
  if (n < 0 || k < 0) {
    stop("Fehler: Sowohl 'n' als auch 'k' dürfen nicht-negativ sein.")
  }
  
  # Überprüfen, ob die Eingabe eine gültige Zahl ist.
  if (!is.numeric(iterationen) || length(iterationen) != 1 || iterationen <= 0) {
    stop("Fehler: Bei der Eingabe muss es sich um eine positive ganze Zahl handeln!")
  }
  
  # Überprüfen, ob "n" kleiner als "k" ist.
  if (n < k) {
    stop("Fehler: n muss größer oder gleich k sein!")
  }
  
  ereignisZaehler <- 0
  for (i in 1:iterationen) {
    x <- sample(1:n)
    if (sum(x == 1:n) >= k) {
      ereignisZaehler <- ereignisZaehler + 1
    }
  }
  
  mu <- ereignisZaehler / iterationen
  return(mu)
}


# Beispiel 1
test_that("Test mit n = 1 und k = 1", {
  expect_equal(wichtelUnglueck(1, 1), 1)
})

# Beispiel 2
test_that("Test mit ungültigen Iterationen", {
  expect_error(wichtelUnglueck(5, 2, iterationen = "ganz, ganz viele"))
})

# Testet, ob ein Fehler auftritt, wenn die Anzahl der Personen kleiner als die Mindestanzahl von Personen ist, die ihr eigenes Geschenk ziehen sollen.
test_that("Test mit n kleiner als k", {
  expect_error(wichtelUnglueck(2, 3))
})

# Testet, ob ein Fehler auftritt, wenn negative Werte für "n" oder "k" eingegeben werden.
test_that("Test mit negativem n oder k", {
  expect_error(wichtelUnglueck(-5, 3))
  expect_error(wichtelUnglueck(5, -3))
})

# Testet, ob die Funktion 0 zurück gibt, wenn sowohl "n" als auch "k" gleich 0 sind, was bedeutet, dass niemand sein Geschenk zieht.
test_that("Test mit n = 0 und k = 0", {
  expect_equal(wichtelUnglueck(0, 0), 0)
})

# Testet, ob die Funktion auch bei einer hohen Anzahl von Iterationen korrekt funktioniert und einen numerischen Wert zurück gibt.
test_that("Test mit hoher Anzahl von Iterationen", {
  expect_is(wichtelUnglueck(10, 1, 10000), "numeric")
})
