#' Aquifer storativity.
#' 
#' A collection of functions used to estimate the storativity and
#' specific storage capacity of an idealized aquifer.
#'
#' @param Diffusiv numeric; the diffusivity, with units \eqn{[X]}
#' @param Conductiv numeric; the hydraulic conductivity, with units \eqn{[X]}
#' @param Transmiss numeric; the transmissivity, with units \eqn{[X]}
#' @return numeric
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{hydrogeo}}, 
#' \code{\link{hydraulic_conductivity}},
#' \code{\link{hydraulic_diffusivity}}
storativity <- function(Transmiss, Diffusiv){
  # Roeloffs 1996, eq 13
	# diff equiv T/S --> S=T/D
	return(Transmiss / Diffusiv)
}
#' @rdname storativity
#' @export
specific_storage <- function(Transmiss, Diffusiv, Conductiv){
  Storativ <- storativity(Transmiss, Diffusiv)
  # Roeloffs 1996, p157
  return(Storativ / Conductiv)
}
#