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
#' @seealso \code{\link{hydrogeo.p-units}}, \code{\link{hydrogeo.p}}
#'  
#' @examples
#' ### code to be run
#' print(dimensional_units(verbose=FALSE)) # show all
#' dimensional_units("diffusivity") # [ L^2 / T ]
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
    quant.unit <- paste("[",quant.unit,"]")
    if (verbose) message(sprintf("%25s %-22s", quant, quant.unit))
    return(invisible(matrix(c(quant, quant.unit), ncol=2)))
  }
}

#' Conversion of units.  
#' 
#' Convert within commonly found units.
#' 
#' There is a panoply of units in hydrogeology literature, so
#' this represents a very small fraction of possibilities.  There are 
#' generally three sets of conversion tools here:
#' 
#' \itemize{
#' \item{length}{ convert meters to feet, etc}
#' \item{permeability}{ convert from darcies to SI, etc; these will begin with
#' \code{to_}. For example, \code{to_msquared} converts darcies to \eqn{m^2}.}
#' \item{pressure}{ convert from pascal to bars, etc}
#' }
#' 
#' @name hydrogeo.p-units
#' @rdname hydrogeo.p-units
#' @param atm numeric; pressure, in standard atmospheres
#' @param bar numeric; pressure in bars
#' @param bbl numeric; US oil barrels (1 bbl ~ 42 gallons US liquid)
#' @param ft numeric; length in feet
#' @param hpa numeric; pressure in hecto-Pascals
#' @param kpa numeric; pressure in kilo-Pascals
#' @param m numeric; length in meters
#' @param pa numeric; pressure in Pascals
#' @param Perm_sqm numeric; permeability, in \eqn{[m^2]}
#' @param Perm_Darcy numeric; permeability, in darcies
#' 
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{dimensional_units}}, \code{\link{hydrogeo.p}}
NULL

#' @rdname hydrogeo.p-units
#' @export
to_msquared <- function(Perm_Darcy){
  sc <- 1 / to_darcies(1) # 1 / (9.869233e13)
  return(sc * Perm_Darcy)
}

#' @rdname hydrogeo.p-units
#' @param milli logical; should the result be divided by 1000?
#' @export
to_darcies <- function(Perm_sqm, milli=FALSE){
  const <- get_constants()
  sc <- const$atm$bar * 10^12 #1.01325e+12
  if (milli) sc <- sc / 1e3
  return(sc * Perm_sqm)
}

#' @rdname hydrogeo.p-units
#' @export
to_millidarcies <- function(Perm_sqm){
  return(to_darcies(Perm_sqm, milli=TRUE))
}

#' @rdname hydrogeo.p-units
#' @export
to_cmsquared <- function(Perm_sqm){
  const <- get_constants()
  sc <- const$conversions$sqm2sqcm
  return(sc * Perm_sqm)
}


#' @rdname hydrogeo.p-units
#' @export
bbl2cm <- function(bbl){
  # oil barrel to cubic meter
  bbl / 6.2898107704
}

#' @rdname hydrogeo.p-units
#' @export
ft2m <- function(ft){ft*1200/3937} # http://www.ngs.noaa.gov/PUBS_LIB/FedRegister/FRdoc59-5442.pdf

#' @rdname hydrogeo.p-units
#' @export
m2ft <- function(m){m/ft2m(1)}

#' @rdname hydrogeo.p-units
#' @export
hpa2pa <- function(hpa){hpa*100}

#' @rdname hydrogeo.p-units
#' @export
pa2hpa <- function(pa){pa/hpa2pa(1)}

#' @rdname hydrogeo.p-units
#' @export
kpa2pa <- function(kpa){kpa*1000}

#' @rdname hydrogeo.p-units
#' @export
pa2kpa <- function(pa){pa/kpa2pa(1)}

#' @rdname hydrogeo.p-units
#' @export
bar2pa <- function(bar){bar/pa2bar(1)}

#' @rdname hydrogeo.p-units
#' @export
pa2bar <- function(pa){pa2hpa(pa)/1000}

#' @rdname hydrogeo.p-units
#' @export
atm2bar <- function(atm){
  const <- get_constants()
  sc <- const$atm$bar
  return(sc * atm)
} 
