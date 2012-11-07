#' function does this
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
#' @name NAMEOFFUNC
#' export
#'
#'
#' @param x  scalar, representing X with units \eqn{[m]}
#'
#' @return scalar, representing Y with units \eqn{[m]}
#' 
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
#