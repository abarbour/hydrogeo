#
transmissivity <- function(Storativ, Diffusiv){
  # Roeloffs 1996, eq 13
  return(Storativ * Diffusiv)
}
#
effective_aquifer_thickness_km <- function(Transmiss, Conductiv){
  # Roeloffs 1996, p 157
	return(Transmiss / Conductiv / 1e3)
}
#