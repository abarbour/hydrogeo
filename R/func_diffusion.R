#' Hydraulic diffusivity.
#' 
#' A collection of various functions used to estimate hydraulic diffusivity, or
#' characteristic scales associated with fluid diffusion in porous media.
#' 
#' Because diffusivity is scale and flow-law 
#' dependent, there are many definitions. These
#' include, but are not limited to:
#' \describe{
#'   \item{\code{\link{hydraulic_diffusivity}}}{\deqn{T / S}}
#'   which uses transmissivity and storativity (careful, this can be problematic); an 
#'   alternate definition:
#'   \item{\code{\link{hydraulic_diffusivity_2}}}{\deqn{C / S_s}}
#'   which uses hydraulic conductivity and specific storage; another alternate
#'   defintion:
#'   \item{\code{\link{hydraulic_diffusivity_3}}}{\deqn{x^2 / 4 \pi t}}
#'   which based on a harmonic perturbation of pore fluids in a sphere
#'   (Biot's second compressional wave); (note that this definition provides
#'   an upper bound on diffusivity;) and yet another definition:
#'   \item{\code{\link{hydraulic_diffusivity_poro}}}{}
#'   which is based on linear poroelasticity.
#' }
#' The functions \code{\link{diffusivity_length}}
#'   and \code{\link{diffusivity_time}}
#'   are rearrangements of \code{\link{hydraulic_diffusivity_3}}.
#'
#' @param Conductiv numeric; the hydraulic conductivity, with units \eqn{[X]}
#' @param Diffusiv numeric; the diffusivity, with units \eqn{m^2/s}
#' @param Permeab numeric; the permeability, with units \eqn{m^2}
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
#' @seealso \code{\link{hydrogeo.p}}, 
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
#' @examples
#' # Length and time (SI)
#' hydraulic_diffusivity_3(pi*2)/pi  # = 1
hydraulic_diffusivity_3 <- function(Length.m=1, Time.s=1){
  # Shapiro Huenges and Borm (1997) Eq 6
  return(Length.m * Length.m / 4 / pi / Time.s)
}

#' @rdname hydraulic_diffusivity
#' @export
hydraulic_diffusivity_poro <- function(Permeab, B., Beta, nu_u=NULL, nu=NULL){
  # Rojstaczer and Agnew 1989, eq 23
  # [ Rice and Cleary 1976 eq X]
  if (is.null(nu_u)){
    const <- get_constants()
    nu_u <- const$Poisson$nu_u
  }
  stopifnot(nu_u>=0 & nu_u<=1)
  if (is.null(nu)){
    const <- get_constants()
    nu <- const$Poisson$nu
  }
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
  # Shapiro Huenges and Borm (1997) Eq 7
  return(sqrt(4 * pi * Diffusiv * Time.s))
}

#' @rdname hydraulic_diffusivity
#' @param Length.m numeric; the length scale, in \eqn{[m]}, to calcuate 
#' characteristic diffusion time for.
#' @export
diffusivity_time <- function(Diffusiv, Length.m=1){
  # Shapiro Huenges and Borm (1997) Eq 5
  #L = sqrt(4 pi D T) so T=L^2/4/pi/D
  return(Length.m * Length.m / 4 / pi / Diffusiv)
}
