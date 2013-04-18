#' Calculate the kinematic viscosity of a fluid.
#' 
#' The kinematic viscosity of a homogeneous body or fluid is defined as the
#' value of its dynamic viscosity divided by its density.  This function 
#' can also divide by gravity, which would be necessary in the computation of, 
#' for example, hydraulic conductivity.
#'
#' @param dyn_visc  scalar; representing the dynamic fluid viscosity, with units \eqn{[Pa \cdot s]}
#' @param fluid_dens  scalar; representing the fluid density, with units \eqn{[kg m^{-3}]}
#' @param grav.divide  logical; should the kinematic viscosiy be divided by standard gravitational acceleration?
#' @param verbose logical; should messages be displayed?
#' @return scalar, a form of kinematic viscosity with units or either:
#' \itemize{
#' \item{\eqn{[m^2 s^{-1}]} if \code{grav.divide==FALSE}}
#' \item{\eqn{[m s]} if \code{grav.divide==TRUE}}
#' }
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{hydrogeo}}, \code{\link{hydraulic_conductivity}}
#'  
#' @examples
#' kinvisc()	# use defaults (water at STP)
#' kinvisc(verbose=TRUE)	# no message
#' kinvisc(grav.divide=TRUE)	# divide by standard gravity
#' kinvisc(grav.divide=TRUE, verbose=TRUE)	# message
#
kinvisc <- function(dyn_visc=NULL, 
                    fluid_dens=NULL, 
                    grav.divide=FALSE,
                    verbose=FALSE){
  const <- hydrogeo:::.constants
  if (is.null(dyn_visc)) dyn_visc <- const$dyn_visc
  if (is.null(fluid_dens)) fluid_dens <- const$fluid_dens
	# kin visc defined as dynamic visc over density
	mu. <- dyn_visc
	rho. <- fluid_dens
	nu. <- mu. / rho. # units of [m**2/s]
	if (grav.divide){
	  # divided by grav is [m**2 s**2/ s m] --> [m s]
    grav <- const$gravity
	  nu. <- nu. / grav
	  if (verbose) message("kinem-visc. [m**2 / s] divided by grav.; units are now [m * s]")
	}
	return(nu.)
}
#
