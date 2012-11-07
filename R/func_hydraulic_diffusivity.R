#
hydraulic_diffusivity <- function(Transmiss, Storativ){
	return(Transmiss / Storativ)
}
#
diffusivity_length <- function(Diffusiv, Time){
  #Lachenbuch 1980
  return(sqrt(4 * Diffusiv * Time))
}
#