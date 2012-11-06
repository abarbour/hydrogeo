##
## Functions dealing with permeability calculations
##
#
## conversion to mdarc from m**2
cmsquared <- function(Perm_m2){
	return(Perm_m2 * 1e4)
}
millidarcies <- function(Perm_m2){
	return(Perm_m2 / 0.987e-12)
}
## kinematic viscosity
kinvisc <- function(dynamic_fluid_visc=1.002e-3, fluid_dens=1000, 
	grav.divide=FALSE,
	verbose=FALSE){
	# kin visc defined as dynamic visc over density, in
	# units of [m**2/s]
	# divided by grav is [m**2 s**2/ s m] --> [m s]
	mu. <- dynamic_fluid_visc
	rho. <- fluid_dens
	nu. <- mu. / rho.
	if (grav.divide){
	  nu. <- nu./9.81
	  if (verbose) message("kinem-visc. [L**2/T] divided by grav.; units are now [L*T]")
	}
	return(nu.)
}
##
hydraulic_diffusivity <- function(Transmiss, Storativ){
	return(Transmiss / Storativ)
}
hydraulic_conductivity <- function(Permeab){
	KVG <- kinvisc(grav.divide=TRUE)
	return(Permeab / KVG)
}
effective_thickness_km <- function(Transmiss, Permeab){
	return(Transmiss / Permeab / 1e3)
}
specific_storage <- function(Storativ, Permeab){
	return(Storativ / Permeab)
}
storativity <- function(Transmiss,Diffusiv){
	# diff equiv T/S --> S=T/D
	return(Transmiss / Diffusiv)
}
##
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
