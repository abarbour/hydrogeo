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
#' @name NAMEOFFUNC
#' export
#'
#' @param x  scalar, representing X with units \eqn{[m]}
#'
#' @return scalar, representing Y with units \eqn{[m]}
#' 
#'
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
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
diffusivity_length <- function(Diffusiv, Time.s=1){
  # Lachenbuch 1980 eq 43
  return(sqrt(4 * Diffusiv * Time.s))
}
#
diffusivity_time <- function(Diffusiv, Length.m=1){
  # inverted, Lachenbuch 1980 eq 43
  #L = sqrt(4 D T) so T=L^2/4/D
  return(Length.m * Length.m / 4 / Diffusiv)
}
#