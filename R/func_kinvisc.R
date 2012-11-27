#' Calculate the kinematic viscosity
#' 
#' The main function to be used 
#' is \code{\link{some_func}}
#' 
#' There are also two helper functions included: 
#' \describe{
#' \item{\code{\link{some_other_func}}}{ to do something.}
#' }
#'
#'
#' @name kinvisc
#' export
#'
#' @param dvisc  scalar, representing the dynamic fluid viscosity, with units \eqn{[Pa \cdot s]}
#' @param fdens  scalar, representing the fluid density, with units \eqn{[kg m^{-3}]}
#' @param grav.divide  logical, should the kinematic viscosiy be divided by standard gravitational acceleration?
#' @param verbose logical, should messages be displayed (\code{TRUE}), or not (\code{FALSE})?
#'
#' @return scalar, a form of the kinematic viscosity, with units of either \eqn{[m^2 s^{-1}]} (if \code{grav.divide==FALSE}) or \eqn{[m \cdot s]} (if \code{grav.divide==TRUE})
#' 
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
#' @references Hsieh, P. A., J. D. Bredehoeft, and J. M. Farr (1987),
#' Determination of aquifer transmissivity from Earth tide analysis,
#' \emph{Water Resour. Res.}, \strong{23} (10), 1824-1832, doi:10.1029/WR023i010p01824.
#' 
#' @references \url{http://www.agu.org/pubs/crossref/1987/WR023i010p01824.shtml}
#'
#' @seealso \code{\link{some_function}}, \code{\link{some_other_func}}
#'  
#' @examples
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
	# kin visc defined as dynamic visc over density, in
	# units of [m**2/s]
	# divided by grav is [m**2 s**2/ s m] --> [m s]
	mu. <- dvisc
	rho. <- fdens
	nu. <- mu. / rho.
	if (grav.divide){
	  nu. <- nu./9.80665
	  if (verbose) message("kinem-visc. [m**2 / s] divided by grav.; units are now [m * s]")
	}
	return(nu.)
}
#