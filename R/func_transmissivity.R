#' Aquifer transmissivity.
#' 
#' A collection of functions used to estimate the transmissivity of
#'  an idealized aquifer system, or the effective length scale (thickness)
#'  of an aquifer.
#'
#' @param Diffusiv numeric; the diffusivity, with units \eqn{[X]}
#' @param Conductiv numeric; the hydraulic conductivity, with units \eqn{[X]}
#' @param Storativ numeric; the storativity, with units \eqn{[X]}
#' @return numeric
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{hydraulic_diffusivity}}, \code{\link{hydrogeo-constants}},
#' \code{\link{hydrogeo}}
transmissivity <- function(Storativ, Diffusiv){
  # Roeloffs 1996, eq 13
  return(Storativ * Diffusiv)
}

#' @details \code{\link{transmissivity}} calculates the thickness, \eqn{[m]}
#' @rdname transmissivity
#' @export
effective_aquifer_thickness <- function(Storativ, Diffusiv, Conductiv){
  Transmiss <- transmissivity(Storativ, Diffusiv)
  # Roeloffs 1996, p 157
	return(Transmiss / Conductiv)
}
#