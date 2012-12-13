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
<<<<<<< HEAD
#' @param dynamic_fluid_visc scalar, the dynamic fluid viscosity in units of 
#' \eqn{[Pa s]} (equivalent to N*·/m**2, or kg/(m s))
#' @param fluid_dens scalar, density of the fluid in units of \eqn{[kg/m^3]}
#' @param grav.divide boolean, should the kinem. visc. be divided by gravitational acceleration?
#  @param verbose boolean, should messages be printed?
#'
#' @return scalar, representing the kinematic viscosity in \eqn{[m**2 / s]} or, 
#' if \code{grav.divide==TRUE} \eqn{[m s]}
#'
=======
#' @param dvisc  scalar, representing the dynamic fluid viscosity, with units \eqn{[Pa \cdot s]}
#' @param fdens  scalar, representing the fluid density, with units \eqn{[kg m^{-3}]}
#' @param grav.divide  logical, should the kinematic viscosiy be divided by standard gravitational acceleration?
#' @param verbose logical, should messages be displayed (\code{TRUE}), or not (\code{FALSE})?
#'
#' @return scalar, a form of the kinematic viscosity, with units of either \eqn{[m^2 s^{-1}]} (if \code{grav.divide==FALSE}) or \eqn{[m \cdot s]} (if \code{grav.divide==TRUE})
#' 
>>>>>>> bf0db9b642358f081b04d56cae17f5e26124f55d
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
#' @seealso \code{\link{hydrogeo}}, \code{\link{hydraulic_conductivity}}
#'  
#' @examples
<<<<<<< HEAD
#' kinvisc()	# use defaults (water at STP)
#' kinvisc(verbose=TRUE)	# no message
#' kinvisc(grav.divide=TRUE)	# divide by standard gravity
#' kinvisc(grav.divide=TRUE, verbose=TRUE)	# message
#
kinvisc <- function(dynamic_fluid_visc=1.002e-3, fluid_dens=1000, 
	grav.divide=FALSE,
	verbose=FALSE){
=======
#' ### code to be run
#' this
#' # or
#' \dontrun{
#' that
#' }
kinvisc <- function(dvisc=1.002e-3, 
                    fdens=1000, 
                    grav.divide=FALSE,
                    verbose=FALSE){
>>>>>>> bf0db9b642358f081b04d56cae17f5e26124f55d
	# kin visc defined as dynamic visc over density, in
	# units of [m**2/s]
	# divided by grav is [m**2 s**2/ s m] --> [m s]
	mu. <- dvisc
	rho. <- fdens
	nu. <- mu. / rho.
	if (grav.divide){
<<<<<<< HEAD
	  nu. <- nu. / 9.80665
	  if (verbose) message("divided result by gravity: units are now [L*T]")
=======
	  nu. <- nu./9.80665
	  if (verbose) message("kinem-visc. [m**2 / s] divided by grav.; units are now [m * s]")
>>>>>>> bf0db9b642358f081b04d56cae17f5e26124f55d
	}
	return(nu.)
}
#
