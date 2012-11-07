#
hydraulic_conductivity <- function(Permeab){
  # Davis & DeWiest 1966, eq 6.10
	KVG <- kinvisc(grav.divide=TRUE)
	return(Permeab / KVG)
}
#