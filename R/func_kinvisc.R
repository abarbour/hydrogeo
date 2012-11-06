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
