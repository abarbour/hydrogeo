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