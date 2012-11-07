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

transmissivity <- function(Storativ, Diffusiv){
  # Roeloffs 1996, eq 13
  return(Storativ * Diffusiv)
}
#
effective_aquifer_thickness_km <- function(Transmiss, Conductiv){
  # Roeloffs 1996, p 157
	return(Transmiss / Conductiv / 1e3)
}
#