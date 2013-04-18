#' Gives dimensional units for quantity desired.
#' 
#' Poroelasticity and hydrogeology can be a mish-mash of units; 
#' keeping them straight can be challenging.  This returns a
#' string with the dimensional units, and (optionally) prints them
#' as a message.
#' 
#' This function uses \code{match.arg} so partial representations are OK.
#' 
#' The dimensional representations this far are
#' \itemize{
#' \item{L}{ length}
#' \item{T}{ time}
#' \item{M}{ mass}
#' }
#' with SI units \eqn{[m]}, \eqn{[s]}, and \eqn{[kg]} respectively.
#'
#' @param quantity  character; the quantity to show units for
#' @param verbose   logical; should a message be printed also?
#' @return A character string with the quantity, and dimensional-units; invisibly.
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{hydrogeo-units}}, \code{\link{hydrogeo}}
#'  
#' @examples
#' ### code to be run
#' print(dimensional_units(verbose=FALSE)) # show all
#' dimensional_units("diffusivity") # 'diffusivity'
#' dimensional_units("diff") # matches also, because of match.arg
#' print(dimensional_units("diff", FALSE)) # print, but no message
#'
dimensional_units <- function(quantity=c("show.all",
                                         "conductivity",
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
  quant.unit <- FALSE
  quant.unit <- switch(quant, 
                       show.all=TRUE,
                       conductivity="L / T", 
                       diffusivity="L^2 / T",
                       transmissivity="L^2 / T",
                       storativity="1 / 1",
                       specific.storage="1 / L",
                       viscosity="M / L / T",
                       density="M / L^3",
                       kinematic.viscosity="L^2 / T",
                       permeability="L^2",
                       force="M * L / T^2",
                       stress="M / L / T^2",
                       strain="1 / 1",
                       compressibility="T^2 * L / M",
                       porosity="{0,1} / 1",
                       skempton="{0,1} / 1"
                       )
  if (is.logical(quant.unit) & quant.unit==TRUE){
    X <- t(sapply(X=quantity[-1], FUN=dimensional_units, verbose=verbose, simplify=TRUE))
    rownames(X) <- NULL
    return(invisible(X))
  } else {
    if (verbose) message(sprintf("  %s  has unit dimensions  [ %s ]", quant, quant.unit))
    return(invisible(matrix(c(quant, quant.unit), ncol=2)))
  }
}

#' Conversion of units.  
#' 
#' Convert within commonly found units.
#' 
#' There is a panoply of units in hydrogeology literature, so
#' this represents a very small fraction of possibilities.
#' 
#' @name hydrogeo-units
#' @rdname hydrogeo-units
#' @param Pressure_atm numeric; pressure, in standard atmospheres
#' @param Perm_sqm numeric; permeability, in \eqn{[m^2]}
#' @param Perm_Darcy numeric; permeability, in darcies
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{dimensional_units}}, \code{\link{hydrogeo}}
NULL

#' @rdname hydrogeo-units
#' @export
to_bars <- function(Pressure_atm){
  sc <- hydrogeo:::.constants$atm2bar
  return(sc * Pressure_atm)
}
#' @rdname hydrogeo-units
#' @export
to_msquared <- function(Perm_Darcy){
  sc <- 1 / to_darcies(1) # 1 / (9.869233e13)
  return(sc * Perm_Darcy)
}
#' @rdname hydrogeo-units
#' @export
to_darcies <- function(Perm_sqm, milli=FALSE){
  sc <- to_bars(10**12) #1.01325e+12
  if (milli) sc <- sc / 1e3
  return(sc * Perm_sqm)
}
#' @rdname hydrogeo-units
#' @export
to_millidarcies <- function(Perm_sqm){
  return(to_darcies(Perm_sqm, milli=TRUE))
}
#' @rdname hydrogeo-units
#' @export
to_cmsquared <- function(Perm_sqm){
  sc <- hydrogeo:::.constants$sqm2sqcm
  return(sc * Perm_m2)
}