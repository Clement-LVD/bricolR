tester_performance_systeme_echangeur_sol_air <- function(
   longueur_tuyau_m = 40
   , years_of_interest = 2020

    , country = "FRANCE"
    , city_to_srch_for = "PARIS"

    , temperature_souhaitee_degres_C = 18
#on va faire des scénario du besoin => soit apport via le systeme d'échangeur, soit l'extérieur, soit confinement

 , ecart_futile_a_corriger_degres_C = 1
 , longueurs_tuyaux_a_tester_m =  c(40, 60, 80)

, temp_interieure_actuelle_thermostat = NULL #si le user met une valeur on utilise ça ds les calculs sinon la temp exterieure

, temperature_sol_C = 14
  ){
  pacman::p_load(tidyverse)
  cat("\n")
  cat("\r"); cat(" 🌡 OBTENIR LES TEMPÉRATURES. Année :", years_of_interest)

 temp_annuelle <- get_temperatures_annuelles(years_of_interest = years_of_interest,
                                         country = country,city_to_srch_for = city_to_srch_for )

 cat("\n 🌀 TESTER LA PERFORMANCE DU SYSTEME")

tests_temperature_au_quotidien <-  calc_pipe_temp_earth_to_air_exchanger(temperature_sol_c =  temperature_sol_C
  , longueur_tuyau_m = longueur_tuyau_m

 , mode_test_de_temp = T,
 longueurs_tuyaux_a_tester_m = longueurs_tuyaux_a_tester_m
 , mode_test_de_longueurs_tuyau = T
, temperatures_a_tester_c  = temp_annuelle$TEMP)

# ça répond un mega dataframe avec toutes les temp entrées d'air possible
# si on demande que une seule longueur tuyau ou une seule temp il y a autant de lignes que d'obs.
# sinon nbre de lignes = nbre de longueurs tuyaux à tester * 366 (nbre observation de temp ds l'année) !

tests_temperature_au_quotidien$annee_de_reference = years_of_interest
tests_temperature_au_quotidien$lieu_de_reference = paste0(city_to_srch_for, " - ", country)
# resultats <- dplyr::select(tests_temperature_au_quotidien, temperature_entree_air_c, temperature_sortie_air_c)
                           #LES NOMS DOIVENT ETRE STABLES pour l'instant (noms parametrables)

 # si le user n'a pas donné de valeur de temp intérieure à analyser : usage de la temp. exterieure

if( is.null(temp_interieure_actuelle_thermostat)){
  temp_interieure_actuelle_thermostat <- tests_temperature_au_quotidien$temperature_entree_air_c
  }   # sinon on a sa super valeur à passer

tests_temperature_au_quotidien$temp_interieure_actuelle_analysee <- temp_interieure_actuelle_thermostat # le parametre de la fn

# on veut (a) calculer le potentiel de froid ou de chaud [selon le BESOIN cad diff entre la TEMPERATURE actuelle et de CONSIGNE]
# (b)

tests_temperature_au_quotidien <- dplyr::mutate(tests_temperature_au_quotidien,

 #### Définir le besoin en froid ou en chaud ####

  BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle = temperature_souhaitee_degres_C - temp_interieure_actuelle_analysee
    # BESOIN => si positif => on veut CHAUFFER (pex on veut 18 et il fait 16 => on veut +2°)
 # / si négatif on veut raffraichir pour perdre - x degrés (p. ex. on veut 18 mais il fait 20 = on veut -2° C )
, difference_futile_a_corriger =  abs(BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle) < ecart_futile_a_corriger_degres_C

, doit_raffraichir = BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle < 0
, doit_chauffer = !doit_raffraichir

#### APPORTS POSSIBLES EN °C ####
 , APPORT_DEGRES_dehors = (temperature_entree_air_c - temp_interieure_actuelle_analysee)
#si négative dehors amène du froid , sinon il fait + chaud dehors
 # donc si cet APPORT et le BESOIN ONT LE MEME SIGNE => on peut l'utiliser !
 # p. ex. si l'échangeur apporte 16° et qu'il fait 12 (apport échangeur + 4) et qu'on veut 18 (= besoin de +4°)
 # on a carrément le taux d'apport de l'echangeur  / idem pour l'apport exterieur (p. ex. s'il fait 10 : apport -2)
 # DONC ON VEUT VERIFIER LE MEME SIGNE

  ,  APPORT_DEGRES_echangeur_sol_air = (temperature_sortie_air_c - temp_interieure_actuelle_analysee)
#si positif : amène du chaud, sinon du froid :)

#### UTILITÉS DES APPORTS ####
, PERTINENCE_APPORT_AIR_dehors = sign(APPORT_DEGRES_dehors) == sign(BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle)
# ON VEUT LE MËME SIGNE DONC ON pourrait VERIFIEr EN MULTIPLIANT mais ça fait une multiplication inutile

, PERTINENCE_APPORT_AIR_echangeur_sol_air = sign(APPORT_DEGRES_echangeur_sol_air) == sign(BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle)

# donc p. ex. si dehors amène -4 degrés et qu'on à besoin de -4 => tout va bien

, taux_apport_echangeur_par_rapport_au_besoin = dplyr::if_else(PERTINENCE_APPORT_AIR_echangeur_sol_air,
     ( abs(APPORT_DEGRES_echangeur_sol_air) / abs(BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle)) * 100, 0)


, taux_apport_air_dehors_par_rapport_au_besoin = dplyr::if_else(PERTINENCE_APPORT_AIR_dehors,
                                                               ( abs(APPORT_DEGRES_dehors) / abs(BESOIN_DEGRES_A_COMPENSER_diff_temp_souhaitee_et_actuelle)) * 100, 0)

# même s'il est pertinent et qu'il apporte +, c'est peut-être une diff' futile qu'on est en train de pister
, apport_echangeur_futile = abs(APPORT_DEGRES_echangeur_sol_air) < ecart_futile_a_corriger_degres_C
,  apport_dehors_futile = abs(APPORT_DEGRES_dehors) < ecart_futile_a_corriger_degres_C

, apport_echangeur_meilleur =  taux_apport_air_dehors_par_rapport_au_besoin <= taux_apport_echangeur_par_rapport_au_besoin
, apport_air_dehors_meilleur =  taux_apport_air_dehors_par_rapport_au_besoin > taux_apport_echangeur_par_rapport_au_besoin

#par défaut moins de 1° C ne nous intéresse pas
, flux_venant_de_dehors_doit_sallumer = PERTINENCE_APPORT_AIR_dehors  & apport_air_dehors_meilleur &
  !apport_dehors_futile & !difference_futile_a_corriger

# & taux_apport_air_dehors_par_rapport_au_besoin > taux_apport_echangeur_par_rapport_au_besoin


, echangeur_doit_sallumer = PERTINENCE_APPORT_AIR_echangeur_sol_air & apport_echangeur_meilleur &
                            !apport_echangeur_futile & !difference_futile_a_corriger ,


, echangeur_doit_sallumer_pour_raffraichir = echangeur_doit_sallumer & doit_raffraichir
#si on doit perdre des ° => alors on doit raffraichir
, echangeur_doit_sallumer_pour_chauffer = echangeur_doit_sallumer & doit_chauffer

, toute_entree_air_futile =  apport_dehors_futile &  apport_echangeur_futile
# , echangeur_contre_productif =  #il est pertinent si  : !PERTINENCE_ET_COEF_en_faveur_de_lechangeur
 # etc.

 # on a un besoin en FROID (p. ex. s'il fait 2° dehors et que je veux 19 mon besoin c'est raffraichir de "-17°" :s
       )


valeurs_uniques <- apply(X = tests_temperature_au_quotidien, MARGIN = 2
                         , FUN = dplyr::n_distinct)

noms_variables_ac_valeur_unique <- names(valeurs_uniques[which(valeurs_uniques == 1)])


statistiques_globales <- dplyr::group_by(tests_temperature_au_quotidien, longueur_tuyau_m ) %>%

  dplyr::summarise(
    tests_temperature_au_quotidien[1, noms_variables_ac_valeur_unique]

 ,   n_obs_meteo  = dplyr::n()

 , n_jours_echangeur_pertinent = sum(PERTINENCE_APPORT_AIR_echangeur_sol_air == T, na.rm = T)
 , n_jours_air_exterieur_pertinent = sum(PERTINENCE_APPORT_AIR_dehors == T, na.rm = T)

, RESULTATS_CI_ENSUITE_EN_POURCENTAGES_DES_OBS_METEO_FOURNIES = "XXXXXXXX"

, difference_de_temp_futile_a_corriger = sum(difference_futile_a_corriger == T, na.rm = T) /   n_obs_meteo * 100

, echangeur_doit_sallumer = sum(echangeur_doit_sallumer == T, na.rm = T) /   n_obs_meteo * 100

, echangeur_va_raffraichir = sum(echangeur_doit_sallumer_pour_raffraichir == T, na.rm = T) /   n_obs_meteo * 100

, echangeur_va_chauffer = sum(echangeur_doit_sallumer_pour_chauffer == T, na.rm = T) / n_obs_meteo * 100

 , air_exterieur_doit_sallumer =   sum(na.rm = T, flux_venant_de_dehors_doit_sallumer)  /  n_obs_meteo * 100

, arrivees_air_exterieures_toujours_futiles =  sum(na.rm = T,toute_entree_air_futile)  /  n_obs_meteo * 100

, arrivees_air_exterieures_toujours_futiles_ET_VOUS_CHAUFFEZ =  sum(na.rm = T,toute_entree_air_futile[doit_chauffer == T])  /  n_obs_meteo * 100
, arrivees_air_exterieures_toujours_futiles_ET_VOUS_RAFFRAICHISSEZ =  sum(na.rm = T,toute_entree_air_futile[doit_raffraichir == T])  /  n_obs_meteo * 100
, mean_taux_apport_quand_echangeur_allume = mean(taux_apport_echangeur_par_rapport_au_besoin, na.rm = T)
, sd_taux_apport_quand_echangeur_allume = sd(taux_apport_echangeur_par_rapport_au_besoin, na.rm = T)
)

statistiques_globales <- dplyr::mutate_if(.tbl = statistiques_globales, is.numeric, round, 2)

print(knitr::kable(caption = "Résumé du modèle calculé",
                   t( statistiques_globales  ))  )


cat("\nDans " ,
    statistiques_globales$echangeur_doit_sallumer ,
"% des relevés météo étudiés, l'échangeur Sol / Air doit s'allumer : il répond à un besoin
et est + pertinent qu'une VMC (corrigé des jours où l'échangeur est futile)\n"
); cat("Il va raffraichir pour ", statistiques_globales$echangeur_va_raffraichir, "% et chauffer pour ",
       statistiques_globales$echangeur_va_chauffer , "% de jours de l'année", years_of_interest)

cat("\nDans " ,   statistiques_globales$air_exterieur_doit_sallumer ,
    "% des relevés météo étudiés, l'arrivée extérieure (en se passant du système d'échangeur) est + pertinent que l'échangeur (corrigé des jours où l'échangeur est futile)\n"
)

cat("\nToute arrivée d'air extérieure est futile dans",
    statistiques_globales$arrivees_air_exterieures_toujours_futiles
   , "% des relevés météo étudiés\n"
)

return(statistiques_globales) # une table avec un résumé des résultats en pourcentage !

}
