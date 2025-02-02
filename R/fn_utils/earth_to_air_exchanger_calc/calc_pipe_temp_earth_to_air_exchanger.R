
calc_pipe_temp_earth_to_air_exchanger <- function(
    longueur_tuyau_m = 50,
    temperature_entree_air_c = 35,
    debit_air_m3s = 0.01,
    diametre_tuyau_m = 0.02,
    temperature_sol_c = 15,
    coefficient_echange_thermique_w_m2k = 10,
    masse_volumique_air_kg_m3 = 1.2,
    capacite_thermique_air_j_kgk = 1005,
    temperatures_a_tester_c = -10:35,
    longueurs_tuyaux_a_tester_m = NULL,
    mode_test_de_temp = FALSE,
    mode_test_de_longueurs_tuyau = FALSE,
    .verbose = TRUE
) {

  if (is.null(longueurs_tuyaux_a_tester_m)) {
    longueurs_tuyaux_a_tester_m <- seq(20, 200, 20)
  }

  # Fonction pour calculer un test de température ou de longueur

  if (mode_test_de_longueurs_tuyau) {
    return( #pr gérer le cas d'un test de longueur + test de temp', on déclenche la fonction recursivement sans messages
      map_dfr(longueurs_tuyaux_a_tester_m, ~calc_pipe_temp_earth_to_air_exchanger(longueur_tuyau = .x,
                                                                                  mode_test_de_temp = mode_test_de_temp,
                                                                                  temperatures_a_tester_c = temperatures_a_tester_c
                                                                                  , .verbose = F
                                                                                  , temperature_entree_air = temperature_entree_air_c)) # map_dfr va bind_rows automatiquement
    )
  }

  # LE CALCUL EN LUI MEME DECLENCHE PAR CETTE FONCTION :
  if (mode_test_de_temp) { # soit on fait une seule fois, soit plein de fois la sous fonction de calcul ac des temp' différentes :
    return(
      map_dfr(temperatures_a_tester_c, ~calc_pipe_temp_earth_to_air_exchanger(.verbose = .verbose,
        longueur_tuyau = longueur_tuyau_m,temperature_entree_air =  .x))  # direct la sous fonction de calcul pour les tests des températures
    )
  }

  # Si aucun test, calcul simple
  resultats <- basic_calc_pipe_temp_earth_to_air_exchanger(longueur_tuyau = longueur_tuyau_m , temperature_entree_air = temperature_entree_air_c)

  if (.verbose) {
    message("ﮩـﮩﮩـ <=> Longueur du tuyau: ", longueur_tuyau_m, "m / ↨ Diamètre: ", diametre_tuyau_m * 1000, "mm")
    message("🌡 Temp. extérieure: ", temperature_entree_air_c, "°C")
    message("💨 Débit air: ", debit_air_m3s, "m³/s")
    message("⏳ Durée résidence: ", round(resultats$temps_residence_s, 2), "s")
    message("🌡️ Température de sortie de l'air: ", round(resultats$temperature_sortie_air_c, 2), "°C")
    message("♨️ NTU = ", round(resultats$ntu, 2))
  }

  return(resultats)
}
