#' Tools for hydrogeology and poroelasticity.
#' 
#' This package provides tools to calculate quantities
#' commonly found in hydrogeology and poroelasticity studies including,
#' to name a few, hydraultic conductivity, permeability, transmissivity, etc.
#' 
# @section Scientific background:
#
# A bunch of stuff, and inline equation \eqn{r}, and a newline equation:
# \deqn{
# \frac{\partial^2 s}{\partial r^2}= 0
# }
# and some more.
# 
# And more.
#'
#' @docType package
#' @name hydrogeo-package
#' @aliases hydrogeo
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
#' @import vcd robCompositions
#' 
# import refs when ready
#'
#' @seealso \code{\link{some_function}}, \code{\link{some_other_func}}
#'  
NULL
.constants <- list(gravity=9.80665)


#' Ranges of diffusivity for a few types of solid-rock and unconsolidated deposits.
#'
#' In general, hydraulic diffusivities can vary over many orders of magnitude, and
#' laboratory- and field-based estimates often disagree significantly.  This 
#' dataset represents a (limited) compilation
#' of any available sources (including other compilations!), and is meant to 
#' show the range of "typical" values, expressed in SI units: \eqn{[m^2/s]}. I also
#' include values estimated for fault-core material.
#' 
#' \itemize{
#'   \item mat.type   Type of material
#'   \item mat.class  Class
#'   \item mat.state  State
#'   \item d.high     Upper bound on typical values of diffusivity \eqn{[m^2/s]}
#'   \item d.low      Lower bound
#'   \item ref        Reference
#' }
#'
#' The value of \code{ref} gives the source which the diffusivity range is from.
#' Current sources include:
#' \itemize{
#'   \item Do06        Doan et al 2006 (Chelungpu fault, Taiwan)
#'   \item Ro96        Roeloffs 1996 (shown in Ref. fig. 14)
#'   \item Wa00        Wang 2000 (App. C.1)
#'   \item Wi00        Wibberley 2002 (MTL, Japan)
#' }
#'
#' @docType  data
#' @keywords  datasets
#' @name  diffusiv
#' @usage  data(diffusiv)
#' @format  A data frame with XX rows and 6 variables
NULL
