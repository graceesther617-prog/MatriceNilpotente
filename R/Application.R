# SCRIPT D'APPLICATION : ANALYSE DE NILPOTENCE SUR LE DATASET NASA


# 1. Charger le dataset stocké dans le package
data("nasa_clean")


cat(" PIPELINE INDUSTRIEL : ANALYSE DES DONNÉES DE LA NASA\n")


# 2. Appel de la fonction principale (extraction + test de nilpotence)
bilan_nasa <- extract_markov_matrix(
  data = nasa_clean,
  state_col = "Tool_wear_min",   # Variable continue d'usure (convertie en stades)
  status_col = "Machine_failure",# Statut de panne (1/0)
  n_stades = 4                   # 4 niveaux d'usure (Neuf, Léger, Critique...)
)

# 3. Affichage du bilan final
cat("\n1. Sous-matrice Q des états d'usure extraite :\n")
print(bilan_nasa$matrice_Q)

cat("\n2. Résultat du test de nilpotence :", bilan_nasa$est_nilpotent, "\n")

if (bilan_nasa$est_nilpotent) {
  cat("3. Indice de nilpotence k (Horizon max de cycles avant panne totale) :", bilan_nasa$indice_k, "cycles\n")
} else {
  cat("3. La matrice n'est pas strictement nilpotente.\n")
}

