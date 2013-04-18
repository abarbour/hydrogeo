#' Calculate the kinematic viscosity of a fluid.
#' 
#' The kinematic viscosity of a homogeneous body or fluid is defined as the
#' value of its dynamic viscosity divided by its density.  This function 
#' can also divide by gravity, which would be necessary in the computation of, 
#' for example, hydraulic conductivity.
#'
#' @param dvisc  scalar; representing the dynamic fluid viscosity, with units \eqn{[Pa \cdot s]}
#' @param fdens  scalar; representing the fluid density, with units \eqn{[kg m^{-3}]}
#' @param grav.divide  logical; should the kinematic viscosiy be divided by standard gravitational acceleration?
#' @param verbose logical; should messages be displayed?
#' @return scalar, a form of kinematic viscosity with units or either:
#' \itemize{
#' \item{\eqn{[m^2 s^{-1}]} if \code{grav.divide==FALSE}}
#' \item{\eqn{[m s]} if \code{grav.divide==TRUE}}
#' }
#' @export
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
kinvisc <- function(dvisc=1.002e-3, 
                    fdens=1000, 
                    grav.divide=FALSE,
                    verbose=FALSE){
	# kin visc defined as dynamic visc over density, in
	# units of [m**2/s]
	# divided by grav is [m**2 s**2/ s m] --> [m s]
	mu. <- dvisc
	rho. <- fdens
	nu. <- mu. / rho.
	if (grav.divide){
	  nu. <- nu. / 9.80665
	  if (verbose) message("divided result by gravity: units are now [L*T]")
	  nu. <- nu./9.80665
	  if (verbose) message("kinem-visc. [m**2 / s] divided by grav.; units are now [m * s]")
	}
	return(nu.)
}
#
