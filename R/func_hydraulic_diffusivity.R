#
hydraulic_diffusivity <- function(Transmiss, Storativ){
  # Roeloffs 1996, eq 13
  warning("T/S calc doesn't appear to be correct... Why? [ ]")
	return(Transmiss / Storativ)
}
#
hydraulic_diffusivity_2 <- function(Conductiv, SpecificStorage){
  # Roeloffs 1996, eq 13
  return(Conductiv / SpecificStorage)
}
#
diffusivity_length <- function(Diffusiv, Time){
  # Lachenbuch 1980
  return(sqrt(4 * Diffusiv * Time))
}
#