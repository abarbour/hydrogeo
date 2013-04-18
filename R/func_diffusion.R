#' Hydraulic diffusivity.
#' 
#' A collection of various functions used to estimate hydraulic diffusivity, or
#' characteristic length scales.
#' 
#' Because diffusivity is scale dependent, there exists a number of definitions,
#' including
#' 
#' \describe{
#'   \item{\code{\link{hydraulic_diffusivity}}}{following Eq (1)}
#'   \item{\code{\link{hydraulic_diffusivity_2}}}{following Eq (2)}
#'   \item{\code{\link{hydraulic_diffusivity_poro}}}{following Eq (3)}
#'   \item{\code{\link{diffusivity_length}}}{following Eq (4)}
#'   \item{\code{\link{diffusivity_time}}}{following Eq (5)}
#' }
#'
#' @param Diffusiv numeric; the diffusivity, with units \eqn{[X]}
#' @param Conductiv numeric; the hydraulic conductivity, with units \eqn{[X]}
#' @param Storativ numeric; the storativity, with units \eqn{[X]}
#' @param SpecificStorage numeric; the specific storage, with units \eqn{[X]}
#' @param Transmiss numeric; the transmissivity, with units \eqn{[X]}
#' @param B. numeric; Skempton's coefficient \eqn{B}
#' @param Beta numeric; the bulk compressibility
#' @param nu_u numeric; undrained Poisson's ratio
#' @param nu numeric; Poisson's ratio
#' @return numeric
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{hydrogeo}}, 
#' \code{\link{hydraulic_conductivity}}, 
#' \code{\link{transmissivity}}, 
#' \code{\link{skempton}},
#' \code{\link{storativity}}
hydraulic_diffusivity <- function(Transmiss, Storativ){
  # Roeloffs 1996, eq 13
  warning("T/S diffusivity calculation may not be accurate. needs verification.")
  return(Transmiss / Storativ)
}

#' @rdname hydraulic_diffusivity
#' @export
hydraulic_diffusivity_2 <- function(Conductiv, SpecificStorage){
  # Roeloffs 1996, eq 13
  return(Conductiv / SpecificStorage)
}

#' @rdname hydraulic_diffusivity
#' @export
hydraulic_diffusivity_poro <- function(Permeab, B., Beta, nu_u=NULL, nu=NULL){
  # Rojstaczer and Agnew 1989, eq 23
  # [ Rice and Cleary 1976 eq X]
  if (is.null(nu_u)) nu_u <- hydrogeo:::.constants$nu_u
  stopifnot(nu_u>=0 & nu_u<=1)
  if (is.null(nu)) nu <- hydrogeo:::.constants$nu
  stopifnot(nu>=0 & nu<=1)
  stopifnot(B.>=0 & B.<=1)
  Num. <- Permeab * B.**2 * (1 - nu) * (1 - 2*nu) * (1 + nu_u)**2
  Den. <- 3 * Beta * (1 + nu) * (1 - nu_u) * (nu_u - nu)
  return(Num./Den.)
}

#' @rdname hydraulic_diffusivity
#' @param Time.s numeric; the time scale, in \eqn{[s]}, to calcuate 
#' characteristic diffusion length for.
#' @export
diffusivity_length <- function(Diffusiv, Time.s=1){
  # Lachenbuch 1980 eq 43
  #Frictional Heating, Fluid Pressure, and the Resistance to Fault Motion 
  return(sqrt(4 * Diffusiv * Time.s))
}

#' @rdname hydraulic_diffusivity
#' @param Length.m numeric; the length scale, in \eqn{[m]}, to calcuate 
#' characteristic diffusion time for.
#' @export
diffusivity_time <- function(Diffusiv, Length.m=1){
  # inverted, Lachenbuch 1980 eq 43
  #Frictional Heating, Fluid Pressure, and the Resistance to Fault Motion 
  #L = sqrt(4 D T) so T=L^2/4/D
  return(Length.m * Length.m / 4 / Diffusiv)
}
