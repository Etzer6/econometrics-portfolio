 code
# EXERCICE 1 --------------------------------------------------------------
 
 # Installation et chargement des packages nécessaires
install.packages("quantmod", dependencies = TRUE)
library(quantmod)
library(ggplot2)
 library(dplyr)
# 1 Téléchargement des séries depuis FRED
getSymbols(c("GDPC1", "UNRATE", "FEDFUNDS"), src = "FRED")

# Conversion en dataframes avec transformation
PIB <- data.frame(Date = index(GDPC1), PIB = as.numeric(GDPC1)) %>%
mutate(Croissance = (PIB - lag(PIB)) / lag(PIB) * 100)

chomage <- data.frame(Date = index(UNRATE), Chomage = as.numeric(UNRATE))
taux_interet <- data.frame(Date = index(FEDFUNDS), TauxInteret = as.numeric(FEDFUNDS))

# 2 Fonction pour tracer les graphiques
plot_series <- function(data, y, title, color) {
  ggplot(data, aes(x = Date, y = .data[[y]])) +
    geom_line(color = color) +
    labs(title = title, x = "Année", y = y) +
    theme_minimal()
}

# Représentation graphiques

plot_series(PIB, "Croissance", "Taux de croissance du pib américain", "blue")
plot_series(chomage, "Chomage", "Taux de chômage aux États-Unis", "red")
plot_series(taux_interet, "TauxInteret", "Taux d'intérêt directeur de la Fed", "green")




# Exercice 4 --------------------------------------------------------------

# 1. Simulez xt de manière à obtenir un échantillon de taille T = 100.

set.seed(123)
T <- 100  
x <- numeric(T)  
x[1] <- 0.5
x[2] <- 0.5
u <- rnorm(T, mean = 0, sd = 1) 

# Simulation du processus AR(2) : x_t = 0.8 x_{t-1} - 0.3 x_{t-2} + u_t
for (t in 3:T) {
  x[t] <- 0.8 * x[t-1] - 0.3 * x[t-2] + u[t]
}

# Afficher les 10 premières valeurs pour vérification
print(head(x, 10))

# 2- Représentation graphique

plot(x, type="l", col="blue", main="Simulation du processus AR(2)", xlab="Temps", ylab="x_t")


# 3-  Les racines du polynôme caractéristique

poly_coeffs <- c(1, -0.8, 0.3)
roots <- polyroot(poly_coeffs)
print(roots)
print(Mod(roots))  

# 4Tracer l'ACF jusqu'à h = 10
acf(x, lag.max = 10, main="Autocorrélation (ACF) du processus AR(2)")

# Tracer la PACF jusqu'à h = 10
pacf(x, lag.max = 10, main="Autocorrélation partielle (PACF) du processus AR(2)")



