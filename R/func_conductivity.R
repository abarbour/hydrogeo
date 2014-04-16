#' Hydraulic conductivity.
#' 
#' \code{\link{hydraulic_conductivity}} uses permeability \eqn{k}, and the
#' visocity \eqn{\nu} of the fluid, to estimate the effective hydraulic conductivity \eqn{C}. 
#' 
#' From Davis and DeWiest (1966), eq 6.10, hydraulic conductivity is defined as:
#' \deqn{C=\frac{k g}{\nu}}
#'
#' @param Permeab numeric; permeability \eqn{k} with units \eqn{[X]}
#' @param ... additional arguments to \code{\link{kinvisc}}
#' @return numeric; The hydraulic conductivity \eqn{C}, with units \eqn{[X]}
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{kinvisc}}, \code{\link{hydrogeo.p}}
hydraulic_conductivity <- function(Permeab, ...){
  # Davis & DeWiest 1966, eq 6.10
  KVG <- kinvisc(grav.divide=TRUE, ...)
  return(Permeab / KVG)
}
