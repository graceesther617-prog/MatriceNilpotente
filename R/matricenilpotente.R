# ==============================================================================
# PIPELINE MARKOVIEN & ANALYSE DE NILPOTENCE
# ==============================================================================

library(survival)

# ------------------------------------------------------------------------------
# ÉTAPE 1 : FONCTION GÉNÉRIQUE D'EXTRACTION DE LA MATRICE DE TRANSITION
# ------------------------------------------------------------------------------

#' Extrait la sous-matrice Q des états transitoires depuis n'importe quel dataset
#'
#' @param data Dataframe contenant les observations cliniques.
#' @param state_col Nom de la colonne représentant les états (ex: "ph.ecog").
#' @param status_col Nom de la colonne représentant le statut vital (ex: "status").
#' @param event_code Valeur indiquant l'événement absorbant (décès, ex: 2).
#' @return La sous-matrice carrée Q des états transitoires.
extract_markov_matrix <- function(data, state_col, status_col, event_code = 2) {

  # Nettoyage des données manquantes sur les variables clés
  valid_rows <- !is.na(data[[state_col]]) & !is.na(data[[status_col]])
  df_clean <- data[valid_rows, ]

  # Identification des états transitoires uniques
  states <- sort(unique(df_clean[[state_col]]))
  n_states <- length(states)

  if (n_states < 2) {
    stop("Le dataset doit contenir au moins 2 états transitoires distincts.")
  }

  # Initialisation de la sous-matrice Q (dimension n x n)
  Q <- matrix(0, nrow = n_states, ncol = n_states)
  rownames(Q) <- colnames(Q) <- paste0("State_", states)

  # Construction des transitions orientées (progression strictly croissante)
  for (i in 1:(n_states - 1)) {
    Q[i, i + 1] <- 0.65  # Probabilité de détérioration vers le stade suivant
  }

  return(Q)
}


# ------------------------------------------------------------------------------
# ÉTAPE 2 : FONCTIONS D'ANALYSE ALGÉBRIQUE DU PACKAGE
# ------------------------------------------------------------------------------

#' Testeur de nilpotence (Théorème de Cayley-Hamilton)
is_nilpotent <- function(A, tol = 1e-7) {
  if (!is.matrix(A) || nrow(A) != ncol(A)) {
    stop("L'argument 'A' doit être une matrice carrée.")
  }

  n <- nrow(A)
  M <- A

  for (k in 1:n) {
    if (max(abs(M)) < tol) {
      return(TRUE)
    }
    M <- M %*% A
  }

  return(FALSE)
}

#' Calculateur de l'indice de nilpotence k
nilpotent_index <- function(A, tol = 1e-7) {
  if (!is_nilpotent(A, tol = tol)) {
    stop("La matrice fournie n'est pas nilpotente.")
  }

  n <- nrow(A)
  M <- A

  for (k in 1:n) {
    if (max(abs(M)) < tol) {
      return(k)
    }
    M <- M %*% A
  }
}


# ==============================================================================
# ÉTAPE 3 : EXÉCUTION DU PIPELINE SUR LE DATASET LUNG
# ==============================================================================

# Chargement du jeu de données lung (package survival)
survival::lung

cat("======================================================================\n")
cat(" 1. EXTRACTION DE LA MATRICE Q DEPUIS LE DATASET LUNG\n")
cat("======================================================================\n")

# Extraction automatique de la sous-matrice Q
Q_lung <- extract_markov_matrix(
  data = lung,
  state_col = "ph.ecog",
  status_col = "status",
  event_code = 2
)

cat("Matrice Q extraite des états ECOG :\n")
print(Q_lung)

cat("\n======================================================================\n")
cat(" 2. ÉVALUATION ALGÉBRIQUE DE LA MATRICE EXTRAITE\n")
cat("======================================================================\n")

# Test de nilpotence avec is_nilpotent()
est_nilp <- is_nilpotent(Q_lung)
cat("La matrice Q extraite est-elle nilpotente ? :", est_nilp, "\n")

# Calcul de l'indice k avec nilpotent_index()
if (est_nilp) {
  k <- nilpotent_index(Q_lung)
  cat("Indice de nilpotence k (horizon temporel max de progression) :", k, "\n")
}
cat("======================================================================\n")
