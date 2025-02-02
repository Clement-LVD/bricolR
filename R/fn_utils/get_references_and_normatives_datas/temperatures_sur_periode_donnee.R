temperatures_annuelles <- function(
 years_of_interest = 2020  
  ,country = "FRANCE"
  , city_to_srch_for = "PARIS"
  ){
  
pacman::p_load(GSODR)
  
  load(system.file("extdata", "isd_history.rda", package = "GSODR"))
  
  if(!country %in% isd_history$COUNTRY_NAME){
    cat("LE PAYS DEMANDÉ N'EST PAS RÉFÉRENCÉ : voici la liste des pays avec stations météo disponibles :\n")
    cat(sep = " / ", unique(isd_history$COUNTRY_NAME))
    return(NA)
  }
  
  Oz <- subset(isd_history, COUNTRY_NAME == country) #liste des stations
 
  n_stations_pays <- nrow(Oz)
  cat("\n 🗺", country, ": vous avez accès à", n_stations_pays, "stations météo" )
  
 villes_trouvees <- subset(Oz, grepl(city_to_srch_for, NAME))
  
n_villes_trouvees <- nrow(villes_trouvees)
  
if(n_villes_trouvees == 0){
   cat("Je n'ai pas trouvé de stations météo avec ce nom de ville : ", city_to_srch_for ,"\n")
  cat("Villes disponibles : \n"); cat(sep = "\n", unique(Oz$NAME))
    return(NA)
}

if(n_villes_trouvees > 1){ cat("\n => Plusieurs stations trouvées à",city_to_srch_for , ": Je prends la plus basse\n") }

station_la_plus_basse <- villes_trouvees[order(villes_trouvees$`ELEV(M)`, decreasing = F)[1] ]
# et on répond la station la plus basse par défaut
cat("Station météo de référence : ", station_la_plus_basse$NAME)
releves <- GSODR::get_GSOD(years =years_of_interest, station =  station_la_plus_basse$STNID)

cat("\nNombre d'observations : ", nrow(releves))
# p.ex. : dplyr::select(releves, TEMP, MIN, MAX, PRCP, dplyr::starts_with("I_"))

return(releves )
}
