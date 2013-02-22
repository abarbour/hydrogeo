#
well_permeab <- function(Transmiss, Len_screen, stdErrors=NULL){
	KVG <- kinvisc(grav.divide=TRUE)
	Perm. <- Transmiss * KVG / Len_screen
	toret <- list(Permeab=Perm., Permeab.stderr=NA)
	if (!(NA %in% stdErrors) & !(is.null(stdErrors))){
	  E. <- as.vector(stdErrors)
	  stopifnot(length(E.)==2)
	  toret$Permeab.stderr=well_permeab_err(Transmiss, E.[1], Len_screen, E.[2])
	}
	return(toret)
}
#