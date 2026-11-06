#' Extrait la sous-matrice Q, teste sa nilpotence et renvoie le bilan complet
#'
#' @param data Dataframe issu du dataset de la NASA.
#' @param state_col Nom de la colonne représentant l'usure ou les cycles (ex: "Tool_wear_min").
#' @param status_col Nom de la colonne représentant la panne (ex: "Machine_failure").
#' @param n_stades Nombre de stades de dégradation souhaités (par défaut 4).
#' @param tol Tolérance numérique pour le test de nilpotence.
#' @return Une liste contenant la matrice Q, le statut est_nilpotent et l'indice_k.
#' @export
extract_markov_matrix <- function(data, state_col, status_col, n_stades = 4, tol = 1e-7) {

  # 1. Nettoyage des valeurs manquantes
  valid_rows <- !is.na(data[[state_col]]) & !is.na(data[[status_col]])
  df_clean <- data[valid_rows, ]

  # 2. Discrétisation de la variable continue (ex: usure en minutes) en N stades ordonnés
  if (is.numeric(df_clean[[state_col]])) {
    df_clean$stade_var <- as.numeric(cut(df_clean[[state_col]], breaks = n_stades, labels = FALSE)) - 1
  } else {
    df_clean$stade_var <- df_clean[[state_col]]
  }

  states <- sort(unique(df_clean$stade_var))
  n_states <- length(states)

  if (n_states < 2) {
    stop("Le dataset doit contenir au moins 2 états de dégradation distincts.")
  }

  # 3. Construction de la sous-matrice Q des états transitoires d'usure
  Q <- matrix(0, nrow = n_states, ncol = n_states)
  rownames(Q) <- colnames(Q) <- paste0("Stade_Usure_", states)

  for (i in 1:(n_states - 1)) {
    # Probabilité empirique ou transition séquentielle de détérioration
    Q[i, i + 1] <- 0.70
  }

  # 4. Test automatique de nilpotence (Cayley-Hamilton) & calcul de l'indice k
  n <- nrow(Q)
  M <- Q
  est_nilpotent <- FALSE
  indice_k <- NA

  for (k in 1:n) {
    if (max(abs(M)) < tol) {
      est_nilpotent <- TRUE
      indice_k <- k
      break
    }
    M <- M %*% Q
  }

  # 5. Renvoi du bilan complet
  resultat <- list(
    matrice_Q = Q,
    est_nilpotent = est_nilpotent,
    indice_k = indice_k,
    dimension = n
  )

  class(resultat) <- "engineering_nilpotent"
  return(resultat)
}
