calc_temp_for_pipe_earth_to_air_exchanger <- function(
    .verbose = T
 # 2 parametres intéressants : longueur du tuyau et température ext. (= par défaut ds l'habitat)
 , longueur_tuyau_m = 50 # va certainement varier => la fonction gère ça
  , temperature_entree_air_C = 35  #va certainement varier => c'est le but de la fonction
 
   , debit_air_m3s =0.01  # valeur de puissance VMC (supposée stable mais influence le temps ou l'air reste ss le sol)
 , diametre_tuyau_m = 0.02 # diametre du tuyau : supposé stable
 # La suite c'est des constantes mais on peut faire varier légèrement si besoin  
 , temperature_sol_C = 15  #selon la profondeur du tuyau on retrouve + ou - de temp'                                 
 , coefficient_echange_thermique_W_m2K = 10 
 , masse_volumique_air_kg_m3 = 1.2 
 , capacite_thermique_air_J_kgK = 1005
 , temperatures_a_tester_C =  -10:35
 , longueurs_tuyaux_a_tester_m =  NULL 
 ,  mode_test_de_longueurs_tuyau = F # doit rester à F par défaut
 , mode_test_de_temp = F #doit rester à F par défaut
 ) {
  
  if(length(longueur_tuyau_m) > 1 ) {longueurs_tuyaux_a_tester_m <- longueur_tuyau_m} #on va tester ces valeurs multiples !
  
  if(is.null(longueurs_tuyaux_a_tester_m)){ longueurs_tuyaux_a_tester_m =   seq(20, 200, 20)}
  # répond une table simple pour la longueur et la temp demandée, éventuellement une giga-table si on fait des tests de plusieurs temp. et plusieurs longueurs
  
  # déjà on part en boucle si le user veut tester :
  resultat_test_longueurs <- list() # ici sans 's' à la fin => le cas de base c'est la table "resultats" (en final de la fonction)
  
   if(mode_test_de_longueurs_tuyau){
     
     longueurs_a_tester = longueurs_tuyaux_a_tester_m
   
       for(i in seq_along(longueurs_a_tester)){
 long_testee = longueurs_a_tester[i]
 
  # appel recursif de la fonction
 resultat_test_longueurs[[i]] <- calc_temp_for_pipe_earth_to_air_exchanger(longueur_tuyau_m = long_testee, 
                                                              .verbose = F
                                              , mode_test_de_temp = mode_test_de_temp
     , mode_test_de_longueurs_tuyau= F #ON EST EN TRAIN DE LE FAIRE VALEUR PAR VALEUR !
                                              , temperatures_a_tester_C = temperatures_a_tester_C )
 # on a une liste d'entrées "resultat" avec autant de listes qu'on ne testait de longueurs !
    
 }
     return(dplyr::bind_rows(resultat_test_longueurs)  ) # et on retourne ça !
    # et s'il veut on fait une boucle ds la boucle !
   
  } #on SORT de la boucle de tests de longueurs
  
  if(mode_test_de_temp){ #si on tombe sur un temp temp : pouf, test temp !
    
    resultats_par_temp = list()
    
      for(temp_i in seq_along(temperatures_a_tester_C)) { #ici le palier de 1° à 35° par saut de 1°
        
        temp_testee = temperatures_a_tester_C[[temp_i]]
        
 resultats_par_temp[[temp_i]] <-  calc_temp_for_pipe_earth_to_air_exchanger(longueur_tuyau_m = longueur_tuyau_m, 
                                                             .verbose = .verbose 
                                                             , 
                                                          , temperature_entree_air_C = temp_testee) }
      
 return(dplyr::bind_rows(resultats_par_temp)  ) #APRES LA BOUCLE ON REGROUPE !
     
    
  }
  # Calcul du débit massique (kg/s)
  debit_massique <- masse_volumique_air_kg_m3 * debit_air_m3s
  
  # Surface d'échange du tuyau (m²) = Périmètre * Longueur
  surface_echange <- pi * diametre_tuyau_m * longueur_tuyau_m
  
  # Nombre d'unités de transfert (NTU)
  NTU <- (coefficient_echange_thermique_W_m2K * surface_echange) / (debit_massique * capacite_thermique_air_J_kgK)
  
  # Température de sortie calculée
  temperature_sortie_air_C <- temperature_sol_C + 
    (temperature_entree_air_C - temperature_sol_C) * exp(-NTU)
  
  # Calcul du temps de résidence (s)
  aire_transversale <- pi * (diametre_tuyau_m / 2)^2
  temps_residence_s <- (aire_transversale * longueur_tuyau_m) / debit_air_m3s
  
resultats <- list(
 temperature_entree_air_C = temperature_entree_air_C  
 ,  temperature_sortie_air_C = temperature_sortie_air_C 
 
 ,   temps_residence_s = temps_residence_s 
 , temperature_sol_C = temperature_sol_C
 , debit_air_m3s = debit_air_m3s
 , longueur_tuyau_m = longueur_tuyau_m
  , diametre_tuyau_m = diametre_tuyau_m
   , N_unites_de_transfert_NTU = NTU #Nombre d'unités de transfert (NTU)
  )

if(.verbose){
cat("ﮩـﮩﮩـ <=> Longueur du tuyau :", longueur_tuyau_m, "m / ↨ Diametre : ", diametre_tuyau_m*1000,  "mm\n")
cat("🌡 Temp. extérieure : ", temperature_entree_air_C, "° C\n")
cat("💨 Débit air : ", debit_air_m3s, "m3 / seconde   / ")
 cat("⏳ Durée résidence dans le tuyau :", round(resultats$temps_residence_s, 2), "s\n")

cat("🌡️Température de sortie de l'air :", round(resultats$temperature_sortie_air_C, 2), "°C\n")
cat("♨️ NTU =", round(resultats$N_unites_de_transfert_NTU, 2), "\n")
}

return(resultats)
}

   
