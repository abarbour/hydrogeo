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
hydraulic_conductivity <- function(Permeab){
  # Davis & DeWiest 1966, eq 6.10
	KVG <- kinvisc(grav.divide=TRUE)
	return(Permeab / KVG)
}
#
hydraulic_diffusivity <- function(Transmiss, Storativ){
  # Roeloffs 1996, eq 13
  warning("T/S calc doesn't appear to be correct... Why? [ ]")
  return(Transmiss / Storativ)
}
#
hydraulic_diffusivity_2 <- function(Conductiv, SpecificStorage){
  # Roeloffs 1996, eq 13
  return(Conductiv / SpecificStorage)
}
#
hydraulic_diffusivity_poro <- function(Permeab, B., Beta, nu_u=1/2, nu=1/4){
  # Rojstaczer and Agnew 1989, eq 23
  # [ Rice and Cleary 1976 eq X]
  stopifnot(nu_u>=0 & nu_u<=1)
  stopifnot(nu>=0 & nu<=1)
  stopifnot(B.>=0 & B.<=1)
  Num. <- Permeab * B.**2 * (1 - nu) * (1 - 2*nu) * (1 + nu_u)**2
  Den. <- 3 * Beta * (1 + nu) * (1 - nu_u) * (nu_u - nu)
  return(Num./Den.)
}
#
diffusivity_length <- function(Diffusiv, Time){
  # Lachenbuch 1980
  return(sqrt(4 * Diffusiv * Time))
}
#