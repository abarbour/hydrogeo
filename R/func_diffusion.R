#
hydraulic_conductivity <- function(Permeab){
  # Davis & DeWiest 1966, eq 6.10
	KVG <- kinvisc(grav.divide=TRUE)
	return(Permeab / KVG)
}
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
hydraulic_diffusivity_poro <- function(Permeab, B., Beta, nu_u=1/2, nu=1/4){
  # Rojstaczer and Agnew 1989, eq 23
  # [ Rice and Cleary 1976 eq X]
  stopifnot(nu_u>=0 & nu_u<=1)
  stopifnot(nu>=0 & nu<=1)
  stopifnot(B.>=0 & B.<=1)
  Num. <- Permeab * B.**2 * (1 - nu) * (1 - 2*nu) * (1 + nu_u)**2
  Den. <- 3 * Beta * (1 + nu) * (1 - nu_u) * (nu_u - nu)
  return(Num./Den.)
}
#
diffusivity_length <- function(Diffusiv, Time){
  # Lachenbuch 1980
  return(sqrt(4 * Diffusiv * Time))
}
#