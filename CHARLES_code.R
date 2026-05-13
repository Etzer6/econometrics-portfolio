# Chargement des libraries
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)
setwd("C:/Users/couli/Downloads/pack projet 2025 (7)/pack projet 2025")

# Création du Dataframe
df <- read.csv("mastercard_appr.csv", sep=";",header = TRUE)
df_test <- read.csv("mastercard_test.csv", sep = ";")


# 1 Manipulation des données ----------------------------------------------

# Créer une variables sexe binaire ----------------------------------------

df <- df %>%
  mutate(
    SEXE_BIN = ifelse(SEXE == "homme", 1, 0),
    GOLD_NUM = ifelse(GOLD == "oui", 1, 0),

# transformation la de la variable situation familiale  ----------------------------------

    
    CELIB = as.integer(SITUA_F == "celibataire"),
    DIVORCE = as.integer(SITUA_F == "divorce"),
    MARIE = as.integer(SITUA_F == "marie"),
    SEPARE = as.integer(SITUA_F == "separe"),
    ULIBRE = as.integer(SITUA_F == "union libre"),
    VEUF = as.integer(SITUA_F == "veuf"),

# transformation la variable CSP  ----------------------------------
 
    AGRI = as.integer(CSP == "agriculteur"),
    ARTI = as.integer(CSP == "artisan"),
    CADRE = as.integer(CSP == "cadre"),
    EMP = as.integer(CSP == "employes"),
    INA = as.integer(CSP == "inactif"),
    OUV = as.integer(CSP == "ouvriers"),
    PINT = as.integer(CSP == "prof intermediaires"),
    RETR = as.integer(CSP == "retraite"),
    SANSEMP = as.integer(CSP == "sans emploi")
  )

# Création d'une variable GOLD --------------------------------------------

df <- df[, !colnames(df) %in% c("GOLD", "Colonne2", "Colonne3", "Colonne13")]
print(df)

# Vérification des valeurs manquantes NA ----------------------------------

missing_values <- colSums(is.na(df))
missing_values[missing_values > 0]


# Suppression des observations dont l'ancienneté des clients est superieur ou égal a son age

df1 <- df %>%
  filter(ANCIENNETE / 12 <= AGE) %>%
  filter(ID != 331)


# Filtre les observations et les  conserve dont l'endettement est inférieure ou égale à 35%

df2 <- df1 %>%
  filter(TX_ENDET <= 35)

# Afficher la structure 
str(df2)

# Aperçu des premières lignes après transformation
head(df2)

# Vérification des valeurs manquantes 
colSums(is.na(df2))

# 2-STATISTIQUE DESCRIPTIVE

#2-1 STATISTIQUE DESCRIPTIVE DES VARIABLE DES AGES ---------------------------

df2 %>%
  summarise(
    min_age = min(AGE, na.rm = TRUE),
    q1_age = quantile(AGE, 0.25, na.rm = TRUE),
    median_age = median(AGE, na.rm = TRUE),
    mean_age = mean(AGE, na.rm = TRUE),
    q3_age = quantile(AGE, 0.75, na.rm = TRUE),
    max_age = max(AGE, na.rm = TRUE),
    sd_age = sd(AGE, na.rm = TRUE))

# Histogramme de la variable age

hist(df2$AGE, breaks = 20, col = "lightblue", main = "Distribution de l'âge", xlab = "Âge", ylab = "Fréquence")

#2-2 STATISTIQUE DESCRIPTIVE DES VARIABLES ANCIENNETE et MOUV CREDTEUR ---------------------------

summary(df2$ANCIENNETE)
summary(df2$MOUV_CRED)

#Histogramme de la variable ANCIENNETE et MOUV CREDTEUR

par(mfrow = c(1, 2))
hist(df2$ANCIENNETE, breaks = 20, col = "lightblue", main = "Distribution de l'ancienneté", xlab = "Ancienneté (mois)")
hist(df2$MOUV_CRED, breaks = 20, col = "lightgreen", main = "Distribution du mouvement créditeur", xlab = "Mouvement créditeur")

# 2-3 Statistiques des variables binaires et des variables numériques

# Statistiques des variables binaires (sexe, situation familiale, CSP, GOLD_NUM)
summary(df2[, c("SEXE_BIN", "GOLD_NUM", "CELIB", "DIVORCE", "MARIE", "SEPARE", 
                "ULIBRE", "VEUF", "AGRI", "ARTI", "CADRE", "EMP", "INA", "OUV", 
                "PINT", "RETR", "SANSEMP")])

# Statistiques générales des variables numériques
summary(df2[, c("AGE", "ANCIENNETE", "MOUV_CRED", "AVOIRS", "TX_ENDET", 
                "CREDITS", "NB_COMPTES", "SOLDE_CC", "DER_MOUV", "NB_OP", "NBJ_DECOUVERT")])

#2-4 répretation de situation familiale --------------------------------------

# Calcul de la distribution des différentes catégories de situation familiale
prop_data <- data.frame(
  Situation_Familiale = c("Célibataire", "Divorcé", "Marié", "Séparé", "Union Libre", "Veuf"),
  Count = c(sum(df2$CELIB, na.rm = TRUE), sum(df2$DIVORCE, na.rm = TRUE), sum(df2$MARIE, na.rm = TRUE),
            sum(df2$SEPARE, na.rm = TRUE), sum(df2$ULIBRE, na.rm = TRUE), sum(df2$VEUF, na.rm = TRUE))
)

prop_data$Proportion <- prop_data$Count / nrow(df2)

# Affichage de la répartition
print(prop_data)

# Création de l'histogramme
barplot(
  prop_data$Count, 
  names.arg = prop_data$Situation_Familiale, 
  col = "lightblue", 
  main = "Répartition des situations familiales", 
  ylab = "Nombre d'individus",
  las = 2  
)


#2-4 répretation de situation CSP

# Liste des variables qualitatives à analyser
qual_vars <- c("AGRI", "ARTI", "CADRE", "EMP", "INA", "OUV", "PINT", "RETR", "SANSEMP")

# Vérifie si toutes les variables existent dans df2
if (all(qual_vars %in% colnames(df2))) {
  # Calcule les fréquences des catégories
  prop_data <- data.frame(
    Catégorie = qual_vars,
    Count = colSums(df2[, qual_vars], na.rm = TRUE) 
  )
  
  
  prop_data$Proportion <- prop_data$Count / nrow(df2)
  
  # Affiche les résultats
  print(prop_data)

barplot(
  prop_data$Count,
  names.arg = prop_data$Catégorie,
  col = "lightblue",
  main = "Répartition des catégories socio-professionnelles",
  ylab = "Nombre d'individus",
  las = 2 
)


#  3scoring -----------------------------------------------------------------

#Transforme les variables binaires en facteurs


# Si df2 existe, créer df3 à partir de df2
df3 <- df2

# Liste des variables binaires à transformer en facteur
binary_vars <- c("SEXE_BIN", "CELIB", "DIVORCE", "MARIE", "SEPARE", "ULIBRE", "VEUF", 
                 "AGRI", "ARTI", "CADRE", "EMP", "INA", "OUV", "PINT", "RETR", "SANSEMP", "GOLD_NUM")

# Appliquer la transformation à toutes les variables en une seule ligne
df3[binary_vars] <- lapply(df3[binary_vars], as.factor)

# Vérification du résultat
str(df3)

# Crée une Variable SEXE_BIN

# Vérifie si la colonne SEXE existe dans df_test
if ("SEXE" %in% colnames(df_test)) {
  df_test$SEXE_BIN <- ifelse(df_test$SEXE == "homme", 1, 0)
} else {
  print("La colonne SEXE n'existe pas dans df_test.")
}

# Transforme la variable SITUA_F

# Utilisation de mutate() et case_when() pour créer les nouvelles variables
df_test <- df_test %>%
  mutate(
    CELIB = ifelse(SITUA_F == "celibataire", 1, 0),
    DIVORCE = ifelse(SITUA_F == "divorce", 1, 0),
    MARIE = ifelse(SITUA_F == "marie", 1, 0),
    SEPARE = ifelse(SITUA_F == "separe", 1, 0),
    ULIBRE = ifelse(SITUA_F == "union libre", 1, 0),
    VEUF = ifelse(SITUA_F == "veuf", 1, 0)
  )

# Vérification du résultat
head(df_test)

# Transforme la variable CSP

# Utilisation de mutate() pour transformer la variable CSP
df_test <- df_test %>%
  mutate(
    AGRI = ifelse(CSP == "agriculteur", 1, 0),
    ARTI = ifelse(CSP == "artisan", 1, 0),
    CADRE = ifelse(CSP == "cadre", 1, 0),
    EMP = ifelse(CSP == "employes", 1, 0),
    INA = ifelse(CSP == "inactif", 1, 0),
    OUV = ifelse(CSP == "ouvriers", 1, 0),
    PINT = ifelse(CSP == "prof intermediaires", 1, 0),
    RETR = ifelse(CSP == "retraite", 1, 0),
    SANSEMP = ifelse(CSP == "sans emploi", 1, 0)
  )

# Vérification du résultat
head(df_test)

# Sélectionne les variables explicatives

features <- c("AGE", "ANCIENNETE", "MOUV_CRED", "AVOIRS", "TX_ENDET", 
              "CREDITS", "NB_COMPTES", "SOLDE_CC", "DER_MOUV", "NB_OP", 
              "NBJ_DECOUVERT", "SEXE_BIN", "CELIB", "DIVORCE", "MARIE", 
              "SEPARE", "ULIBRE", "VEUF", "AGRI", "ARTI", "CADRE", "EMP", 
              "INA", "OUV", "PINT", "RETR", "SANSEMP")

# Vérifier si toutes les variables de features existent dans df_test
if (all(features %in% colnames(df_test))) {
  print("Toutes les variables explicatives sont présentes dans df_test.")
} else {
  missing_vars <- features[!features %in% colnames(df_test)]
  print(paste("Les variables suivantes sont manquantes :", paste(missing_vars, collapse = ", ")))
}

# Convertis les colonnes en numériques

df3[features] <- lapply(df3[features], as.numeric)

# Convertir en facteur pour la régression logistique

df3$GOLD_NUM <- as.factor(df3$GOLD_NUM) 

# Vérifier la structure des données
str(df3)

#  Supprime les lignes avec des NAs
df3 <- na.omit(df3)
df_test <- na.omit(df_test)

#Séparer les données en variables explicatives X et expliquée Y
X_train <- df3[, features]
y_train <- df3$GOLD_NUM


# Conversion de la variable cible en facteur 
y_train <- as.factor(y_train)

# Vérification des types de données
str(X_train)
str(y_train)

# Normalisation des variables explicatives 

preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProcValues, X_train)

# Vérification des données transformées
summary(X_train_scaled)

# Entraînement du modèle GLM
model <- glm(GOLD_NUM ~ ., data = data.frame(GOLD_NUM = y_train, X_train_scaled), 
             family = binomial)

# Affichage du résumé du modèle
summary(model)

# Normalisation des données de test avec les mêmes paramètres
X_test_scaled <- predict(preProcValues, df_test[, features])

# Prédiction des probabilités
prob_pred <- predict(model, newdata = data.frame(X_test_scaled), type = "response")

# Transformation en score (0-100)
df_test$Score <- round(prob_pred * 100, 2)

# Prédiction sur l'échantillon 
prob_train <- predict(model, newdata = data.frame(X_train_scaled), type = "response")

# Prédiction sur l'échantillon 
prob_train <- predict(model, newdata = X_train_scaled, type = "response")

# Calcul de la courbe ROC et AUC
roc_auc <- roc(y_train, prob_train)

# Afficher l'AUC
cat("AUC du modèle :", auc(roc_auc), "\n")

# Tracer la courbe ROC
plot(roc_auc, main = "Courbe ROC du modèle")


# Sélectionner 50 individus 
df_test_sample <- df_test[1:50, ]

# Normaliser les données
X_test_sample_scaled <- predict(preProcValues, df_test_sample[, features])

# Prédire les scores
df_test_sample$Score <- round(predict(model, newdata = data.frame(X_test_sample_scaled), type = "response") * 100, 2)

# Afficher les résultats
print(df_test_sample[, c("ID", "Score")])
print(df_test_sample)

# Export des résultats
write.csv(df_test_sample, "CHARLES_COULIBALY_prevision.csv", row.names=FALSE)





























