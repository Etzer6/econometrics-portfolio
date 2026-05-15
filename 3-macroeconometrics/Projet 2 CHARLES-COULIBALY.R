#Les codes pour le DM 2 de macroeconomie 

################## EXERCICE 1 ##############################

# question 2

# Définition des coefficients
phi <- c(0.5, -0.3, 0.2)

# Trouver les racines du polynôme caractéristique
roots <- polyroot(c(1, -phi))

# Afficher les racines
print(roots)

# Vérifier la stationnarité
all(Mod(roots) > 1)

# question 3

set.seed(123) 

# Paramètres
T <- 1000
phi1 <- 0.5
phi2 <- -0.3
phi3 <- 0.2
sigma_u <- 1

# Simulation du bruit blanc
u <- rnorm(T, mean = 0, sd = sigma_u)

# Initialisation de Y avec les conditions Y(-2) = Y(-1) = Y(0) = 0
Y <- numeric(T)
Y[1:3] <- 0

# Génération de la série AR(3)
for (t in 4:T) {
  Y[t] <- phi1 * Y[t-1] + phi2 * Y[t-2] + phi3 * Y[t-3] + u[t]
}

# Visualisation de la série
plot(Y, type = "l", main = "Simulation d'un processus AR(3)")

# question 4

# Estimation du modèle AR(3)
model <- arima(Y, order = c(3,0,0), method = "ML")

# Afficher les résultats
summary(model)


################# EXERCICE 2 ##########################################

library(tseries)

# Lire toutes les lignes du fichier
lignes <- readLines("C:/Users/couli/Downloads/EXO2.txt")

# Supprimer la première ligne 
donnees_brutes <- lignes[-1]

# Enlever les guillemets et les espaces superflus
donnees_nettoyees <- gsub('"', '', donnees_brutes)
donnees_nettoyees <- trimws(donnees_nettoyees)    

# Convertir en numérique
donnees <- as.numeric(donnees_nettoyees)
head(donnees) 


# question 1

moyenne <- mean(donnees)
print(paste("Moyenne empirique :", round(moyenne, 4)))

# question 2

acf(donnees, main = "ACF des données EXO2")
pacf(donnees, main = "PACF des données EXO2")

# question 3

modele <- arima(donnees, order = c(1, 0, 0), include.mean = TRUE)
summary(modele)

# question 5

Box.test(residuals(modele), lag = 10, type = "Ljung-Box")


########################## EXERCICE 4  ################################

# question a

set.seed(123) 
n <- 100
theta <- -0.6
u <- rnorm(n, mean = 0, sd = 1)
X <- numeric(n)
X[1] <- u[1]  # u0 = 0
for (t in 2:n) {
  X[t] <- u[t] + theta * u[t-1]
}

# question c

loglik <- function(theta, data) {
  n <- length(data)
  u <- numeric(n)
  u[1] <- data[1]  # u0 = 0
  for (t in 2:n) {
    u[t] <- data[t] - theta * u[t-1]
  }
  -sum(dnorm(u, mean = 0, sd = 1, log = TRUE))
}
result <- optim(par = 0.5, fn = loglik, data = X, method = "BFGS")
theta_hat <- result$par
print(result$par)


######################### EXERCICE 5 ################################

library(readxl)
library(urca)

# question 1

# Importer les données

data <- read_excel("C:/Users/couli/Downloads/EXO3 (1).xls")
years <- data$Année
population <- data$USPOP

# Calculer le logarithme de la population
log_population <- log(population)

# Tracer la série en échelle logarithmique
plot(years, log_population, type = "l", 
     xlab = "Année", ylab = "Log(Population)", 
     main = "Évolution logarithmique de la population américaine (1948-2023)")

# question 2

# Créer une variable de temps (t = 1, 2, ..., 76)
t <- 1:length(years)

# Estimation du modèle quadratique
modele_quad <- lm(log_population ~ t + I(t^2))
summary(modele_quad)

# question 3

# Valeurs ajustées de la tendance
tendance <- fitted(modele_quad)

# Écarts entre log-population et tendance
ecarts <- log_population - tendance

# Graphique 2 : Log-population et tendance
plot(years, log_population, type = "l", col = "blue", 
     xlab = "Année", ylab = "Log(Population)", 
     main = "Log(Population) vs. Tendance Quadratique")
lines(years, tendance, col = "red", lwd = 2)
legend("topleft", legend = c("Données", "Tendance"), 
       col = c("blue", "red"), lty = 1)

# Graphique 3 : Écarts
plot(years, ecarts, type = "l", col = "green", 
     xlab = "Année", ylab = "Écarts", 
     main = "Écarts par rapport à la Tendance")

# question 4

# Test de Ljung-Box pour un ordre 1
Box.test(ecarts, lag = 1, type = "Ljung-Box")

# question 5

# Test ADF avec 1 retard
adf_test <- ur.df(log_population, type = "trend", lags = 1)
summary(adf_test)




coefficients <- c(1, -0.5, 0.3, -0.2)  # Coefficients du polynôme
racines <- polyroot(coefficients)
mod_racines <- Mod(racines)
print(mod_racines)  # Résultat : 1.58, 1.58, 2.50 (toutes > 1)

set.seed(123)
T <- 1000
phi <- c(0.5, -0.3, 0.2)
sigma_u <- 1  # Variance des erreurs
Y <- numeric(T + 3)  # Y[-2], Y[-1], Y[0] initialisés à 0
u <- rnorm(T + 3, mean = 0, sd = sigma_u)

# Simulation récursive
for (t in 4:(T + 3)) {
  Y[t] <- phi[1] * Y[t-1] + phi[2] * Y[t-2] + phi[3] * Y[t-3] + u[t]
}
Y <- Y[4:(T + 3)]  # Garder les 1000 observations

# Estimation avec arima (inclure un intercept si nécessaire)
modele <- arima(Y, order = c(3, 0, 0), include.mean = FALSE)
summary(modele)

