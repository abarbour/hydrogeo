#
effective_thickness_km <- function(Transmiss, Permeab){
	return(Transmiss / Permeab / 1e3)
}
#