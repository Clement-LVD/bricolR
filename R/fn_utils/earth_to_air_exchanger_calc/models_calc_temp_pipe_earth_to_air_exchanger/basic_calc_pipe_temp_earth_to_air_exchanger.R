basic_calc_pipe_temp_earth_to_air_exchanger <- function(longueur_tuyau, temperature_entree_air
                                                         ,     debit_air_m3s = 0.01 # le  ventilateur indiqué par le user
                                                        ,  diametre_tuyau_m = 0.02 # le diametre tuyau
                                                        ,  temperature_sol_c = 15 # profondeur et ds une moindre mesure compo' du sol
                                                        ,  coefficient_echange_thermique_w_m2k = 8
  #coefficient de transfert thermique (représente l'efficacité du tuyau pour échanger de la chaleur avec l'air) -> si les résultats semblent délirants, on peut tweaker ce parametre pour réduire l'efficacité du tuyau

   ,  conductivite_tuyau = 0.5 # Acier : Environ 50 W/m·K / Cuivre : Environ 400 W/m·K / Plastique (comme PVC) : Environ 0.2 W/m·K à 0.5 W/m·K

          ,  masse_volumique_air_kg_m3 = 1.2 # constante sauf si on dispose de l'humidité et de la temp' et de l'altitude idéalement
           , capacite_thermique_air_j_kgk = 1005 # idem que la masse volumique, constante
                                                        ) {

  # Ajuster la conductivité thermique du tuyau en fonction de la température
  if (temperature_sol_c < 0 || temperature_entree_air < 0) {
    conductivite_tuyau <- conductivite_tuyau * 0.5  # Réduire la conductivité thermique dans les environnements très froids
  }

  # Calcul du débit massique
  debit_massique <- masse_volumique_air_kg_m3 * debit_air_m3s
  surface_echange <- pi * diametre_tuyau_m * longueur_tuyau
  ntu <- (coefficient_echange_thermique_w_m2k * surface_echange) / (debit_massique * capacite_thermique_air_j_kgk)

   # Température de sortie basique sans correction (échange thermique entre l'air et le sol)
  temperature_sortie_air_c_basic <- temperature_sol_c + (temperature_entree_air - temperature_sol_c) * exp(-ntu)

  # Calcul de la correction par la conductivité thermique (assure un transfert thermique cohérent)
  difference_temp <- temperature_entree_air - temperature_sol_c

  # Ajuster le transfert thermique basé sur la conductivité du tuyau
  # Si la conductivité du tuyau est faible, l'effet est plus faible.
  # Si le tuyau est très conducteur, l'air se réchauffe ou refroidit plus rapidement.
  correction_conductivite <- conductivite_tuyau * difference_temp / longueur_tuyau

  # Appliquer la correction pour la température de sortie
  temperature_sortie_air_c_avec_conductivite_tuyau <- temperature_sortie_air_c_basic + correction_conductivite

  aire_transversale <- pi * (diametre_tuyau_m / 2)^2
  temps_residence_s <- (aire_transversale * longueur_tuyau) / debit_air_m3s

  tibble(
    longueur_tuyau_m = longueur_tuyau,
    temperature_entree_air_c = temperature_entree_air,
    temperature_sortie_air_c = temperature_sortie_air_c_basic,
    temperature_sortie_air_corrigee_conductivite = temperature_sortie_air_c_avec_conductivite_tuyau,
    temps_residence_s = temps_residence_s,
    ntu = ntu
  )
}

