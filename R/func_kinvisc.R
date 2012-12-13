#' Calculate the kinematic viscosity
#' 
#' The kinematic viscosity of a homogeneous body or fluid is defined as the
#' value of its dynamic viscosity divided by its density.  This function 
#' can also divide by gravity, which would ne necessary in the computation of, 
#' for example, hydraulic conductivity.
#'
#' @name kinvisc
#' export
#'
#' @param dynamic_fluid_visc scalar, the dynamic fluid viscosity in units of 
#' \eqn{[Pa s]} (equivalent to N*·/m**2, or kg/(m s))
#' @param fluid_dens scalar, density of the fluid in units of \eqn{[kg/m^3]}
#' @param grav.divide boolean, should the kinem. visc. be divided by gravitational acceleration?
#  @param verbose boolean, should messages be printed?
#'
#' @return scalar, representing the kinematic viscosity in \eqn{[m**2 / s]} or, 
#' if \code{grav.divide==TRUE} \eqn{[m s]}
#'
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
#' @seealso \code{\link{hydrogeo}}, \code{\link{hydraulic_conductivity}}
#'  
#' @examples
#' kinvisc()	# use defaults (water at STP)
#' kinvisc(verbose=TRUE)	# no message
#' kinvisc(grav.divide=TRUE)	# divide by standard gravity
#' kinvisc(grav.divide=TRUE, verbose=TRUE)	# message
#
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
	  nu. <- nu. / 9.80665
	  if (verbose) message("divided result by gravity: units are now [L*T]")
	}
	return(nu.)
}
#
