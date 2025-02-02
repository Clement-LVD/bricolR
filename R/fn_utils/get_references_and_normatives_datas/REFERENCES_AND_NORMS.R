REFERENCES_AND_NORMS <- function(regex_names_of_ref_to_return = ".*"){
  
  #REF is a NAMED list of French build' references & norms, with dataframes inside each entry
 REF <- list(
   # 1st entry : air-to-air exchange
earth_to_air_exchanger = list(
     
     pipe_diameter = data.frame( 
 flow_meter_cube =c(100, 125, 150, 200, 250, 300) 

, ideal_diameter_mm =  c(28, 35, 42, 56, 69,83)

,minimum_diameter_mm_if_speed_1_point_5_m_per_s =c(100, 150, 150, 150, 200, 200)
  
, maximum_diameter_mm_if_speed_3_point_5_m_per_s =c(150, 150, 200, 200, 250, 250)

 
) ) 
 )

return(REF)
  
  
  
  
}
