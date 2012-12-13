#' This package provides tools to calculate quantities commonly
#' used in hydrogeology and poroelasticity studies.
#' 
#' The main function to be used 
#' is \code{\link{some_func}}
#' 
#' There are also two helper functions included: 
#' \describe{
#' \item{\code{\link{some_other_func}}}{ to do something.}
#' }
#'
#' @section Scientific background:
#'
#' A bunch of stuff, and inline equation \eqn{r}, and a newline equation:
#' \deqn{
#' \frac{\partial^2 s}{\partial r^2}= 0
#' }
#' and some more.
#' 
#' And more.
#'
#' @docType package
#' @name hydrogeo-package
#' @aliases hydrogeo
#' @title Tools which may be useful for hydrogeology and poroelasticity studies.
#' 
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
# @import package
#' 
#' @references Davies, S. N., and DeWiest, R. J. M. (1966), \strong{Hydrogeology}, 
#' \emph{J Wiley and Sons}, \emph{New York}, 61
#' 
#' @references Lachenbruch, A. H. (1980), 
#' Frictional heating, fluid pressure, and the resistance to fault motion, 
#' \emph{J. Geophys. Res.}, \strong{85} (B11), 6097–6112, doi:10.1029/JB085iB11p06097
#' 
#' @references Roeloffs, E. (1996), 
#' Poroelastic Techniques in the Study of Earthquake-Related Hydrologic Phenomena,
#' \emph{Advances in Geophysics}, \strong{37}, 135-195, doi: 10.1016/S0065-2687(08)60270-8
#'
#' @references Rojstaczer, S. and D. C. Agnew (1989), 
#' The influence of formation material properties on the response of water levels in wells to Earth tides and atmospheric loading, 
#' \emph{J. Geophys. Res.}, \strong{94} (B9), 12403–12411, doi:10.1029/JB094iB09p12403
#'
#' @seealso \code{\link{some_function}}, \code{\link{some_other_func}}
#'  
NULL


#' Ranges of diffusivity for a few types of solid-rock and unconsolidated deposits.
#'
#' In general, hydraulic diffusivities can vary over many orders of magnitude, and
#' laboratory- and field-based estimates may disagree.  This dataset is a compilation
#' of any available sources (including other compilations!), and is meant to represent 
#' the range of "typical" values, expressed in SI units: \eqn{[m^2/s]}.
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
#'   \item R96        Roeloffs 1996 (shown in Fig. 14)
#'   \item W00        Wang 2000 (App. C.1)
#' }
#'
#' @docType  data
#' @keywords  datasets
#' @name  diffusiv
#' @usage  data(diffusiv)
#' @format  A data frame with XX rows and 6 variables
NULL
