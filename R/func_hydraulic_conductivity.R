hydraulic_conductivity <- function(Permeab){
	KVG <- kinvisc(grav.divide=TRUE)
	return(Permeab / KVG)
}
