#' @title Gives dimensional units for quantity desired.
#' 
#' Poroelasticity and hydrogeology can be a mish-mash of units; 
#' keeping them straight can be challenging.  This returns a
#' string with the dimensional units, and (optionally) prints them
#' as a message.
#' 
#' The dimensional representations this far are
#' \describe{
#' \item{L}{length}
#' \item{T}{time}
#' \item{M}{mass}
#' }
#'
#' @name dimensional_units
#' @export
#'
#' @param quantity  char, representing the quantity to show units for
#' @param verbose   boolean, should a message be printed also?
#'
#' @return invisible char tuple with quantity and quantity-units
#' 
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
#' @seealso \code{\link{millidarcies}}, \code{\link{cmsquared}}
#'  
#' @examples
#' ### code to be run
#' dimensional_units("diff") #matches 'diffusivity'
#' print(dimensional_units("diff", FALSE))
#'
dimensional_units <- function(quantity=c("conductivity",
                                         "diffusivity",
                                         "transmissivity",
                                         "storativity",
                                         "specific.storage",
                                         "viscosity",
                                         "density",
                                         "kinematic.viscosity",
                                         "permeability",
                                         "force",
                                         "stress",
                                         "strain",
                                         "compressibility",
                                         "porosity",
                                         "skempton"), verbose=TRUE){
  quant <- match.arg(quantity)
  quant.unit <- switch(quant, 
                       conductivity="L / T", # X
                       diffusivity="L^2 / T", # X
                       transmissivity="L^2 / T", # X
                       storativity=" ", # X
                       specific.storage="1 / L", # X
                       viscosity="M / L / T", # X
                       density="M / L^3", # X
                       kinematic.viscosity="L^2 / T", # X
                       permeability="L^2", # X
                       force="M * L / T^2", # X
                       stress="M / L / T^2", # X
                       strain="1 / 1", # X
                       compressibility="T^2 * L / M", # X
                       porosity="{0,1} / 1",
                       skempton="{0,1} / 1"
                       )
  if (verbose) message(sprintf("\t%s dimensional units: [%s]", quant, quant.unit))
  return(invisible(c(quant, quant.unit)))
}
#
millidarcies <- function(Perm_m2){
  return(Perm_m2 / 0.987e-12)
}
#
cmsquared <- function(msquared){
  return(msquared * 1e4)
}
#