#
permeability <- function(Transmiss, Length.scale){
  KVG <- kinvisc(grav.divide=TRUE)
  Perm. <- Transmiss * KVG / Length.scale
  return(Perm.)
}
#
well_permeab <- function(Transmiss, Len_screen, stdErrors=NULL){
  Perm. <- permeability(Transmiss, Len_screen)
	toret <- list(Permeab=Perm., Permeab.stderr=NA)
	if (!(NA %in% stdErrors) & !(is.null(stdErrors))){
	  E. <- as.vector(stdErrors)
	  stopifnot(length(E.)==2)
	  toret$Permeab.stderr=well_permeab_err(Transmiss, E.[1], Len_screen, E.[2])
	}
	return(toret)
}
#
well_permeab_err <- function(Transmiss, Transmiss.stderr, Ls., Ls.stderr){
  # Derived from  T * nu / Ls / g, assuming
  # only T and Ls will have uncertainties,
  # using rules from Taylor 1997.
  T. <- Transmiss
  dT. <- Transmiss.stderr
  dLs. <- Ls.stderr
  KVG <- kinvisc(grav.divide=TRUE)
  A. <- dT./Ls.
  B. <- T. * dLs. / Ls. / Ls.  # orig expression has -1, but taking norm
  dk. <- KVG * norm(cbind(A.,B.),type="F") # Euclidean norm (Froeb)
  return(dk.)
}
#