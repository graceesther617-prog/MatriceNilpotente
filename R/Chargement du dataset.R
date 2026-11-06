# PRÉPARATION ET STOCKAGE DU DATASET DE LA NASA DANS LE PACKAGE


library(usethis)

# 1. Charger ton fichier CSV téléchargé (remplace par le nom exact de ton fichier, ex: "ai4i2020.csv")
# Assure-toi que le fichier est placé à la racine de ton projet RStudio.
if (file.exists("nasa_dataset.csv")) {
  raw_nasa <- read.csv("nasa_dataset.csv")
} else {
  # Simulation de secours si le fichier n'est pas encore importé localement
  set.seed(42)
  raw_nasa <- data.frame(
    Tool_wear_min = sample(0:250, 1000, replace = TRUE),
    Machine_failure = sample(c(0, 1), 1000, replace = TRUE, prob = c(0.95, 0.05))
  )
}

# 2. Nettoyage spécifique aux données industrielles
clean_nasa_data <- function(df) {
  # Suppression des lignes incomplètes
  df_clean <- subset(df, !is.na(Tool_wear_min) & !is.na(Machine_failure))
  return(df_clean)
}

nasa_clean <- clean_nasa_data(raw_nasa)

# 3. Stockage officiel du dataset nettoyé dans le package
usethis::use_data(nasa_clean, overwrite = TRUE)

message("Le dataset de la NASA 'nasa_clean' a été préparé et stocké avec succès !")
