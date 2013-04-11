#' Calculate compressibilities and strain sensitivity.
#' 
#' Calculates Equation (6) from Rojstaczer and Agnew (1989): \eqn{\hat{\beta}}
#' and Equation (10) using strain sensitivity
#' 
#' Assumptions:  
#' The intake of the well is assumed to penetrate a porous elastic 
#' medium with uniform properties. These properties are those specified 
#' by the theory of Biot [1941] (as reexpressed by Rice and Cleary [1976] 
#' and Green and Wang [1986]), namely the compressibilities
#' of the solid phase Beta_u, the fluid phase Beta_f, 
#' and the porous matrix
#' when drained of fluid Beta (the matrix compressibility), 
#' together with the Poisson's ratio nu,
#' of the matrix, the porosity phi, and the permeability kappa.
#' 
#' As noted by Rice and Cleary [1976], this definition for 
#' B assumes that the rock matrix is homogeneous and all the 
#' pore space is interconnected.
#' 
#' @name compressibility
#' @rdname compressibility
#' 
#' @references S. Rojstaczer and D.C. Agnew (1989), 
#' “The Influence of Formation Material Properties on the Response of Water Levels in Wells to Earth Tides and Atmospheric Loading,” 
#' \emph{J. Geophys. Res.}, \strong{94} (B9), pp. 12403-12411.
#'
NULL

#' @rdname compressibility
#' @param Beta numeric; The compressibility of the solid matrix, \eqn{\beta}
#' @param B. numeric; Skempton's coefficient \eqn{B}
#' @param Beta_u numeric; \eqn{\beta} for an undrained state
#' @export
undrained_compressibility.from.beta <- function(Beta, B., 
                                           Beta_u=2e-10){
  alph <- .calc_alpha(Beta, Beta_u)
  Beta_hat <- Beta * (1 - B. * alph)
  return(Beta_hat)
}
#' @rdname compressibility
#' @param As. numeric; areal strain sensitivity \eqn{A_s}
#' @param nu_u numeric; undrained Poisson's ratio \eqn{\nu}
#' @param fluid_dens numeric; the density \eqn{\rho} of the fluid in consideration
#' @param ... \code{\link{.from.areal_strain_sens}}: additional parameters passed to \code{\link{.calc_prat}}
#' @export
undrained_compressibility.from.areal_strain_sens <- function(As., B., ...){
  prat <- .calc_prat(B., ...)
  Beta_hat <- prat / As.
  return(Beta_hat)
}
#' @rdname compressibility
#' @param Beta_hat numeric; \eqn{\hat{\beta}}
#' @param ... \code{\link{.from.undrained_compressibility}}: additional parameters passed to \code{\link{.calc_prat}}
#' @export
areal_strain_sens.from.undrained_compressibility <- function(Beta_hat, B., ...){
  prat <- .calc_prat(B., ...)
  As. <- prat / Beta_hat
  return(As.)
}
#' @rdname compressibility
#' @export
.calc_prat <- function(B., nu_u=0.35, fluid_dens=1000){
  # R and A (89) Eq10*Beta_hat
  stopifnot(nu_u>=0 & nu_u<=1)
  mnu <- 1 - nu_u
  stopifnot(B.>=0 & B.<=1)
  prat <- (mnu - nu_u) * B. / fluid_dens / 9.80665 / mnu
  return(prat)
}

#' @rdname compressibility
#' @param nu numeric; Poisson's ratio \eqn{\nu}
#' @param ... \code{\link{.calc_nu_u}}: additional parameters passed to \code{\link{.calc_alpha}}
#' @export
.calc_nu_u <- function(B., Beta, 
                       nu=0.25,
                       ...){
  # R A 89, eq 9
  stopifnot(nu>=0 & nu<=1)
  stopifnot(B.>=0 & B.<=1)
  alph <- .calc_alph(Beta, ...)
  BNA <- B. * (1 - 2 * nu) * alph
  Num. <- 3 * nu + BNA
  Den. <- 3 - BNA
  return(Num./Den.)
}

#' @rdname compressibility
#' @export
.calc_alpha <- function(Beta, 
                        Beta_u=2e-11){
  # from Rice (1998, notes), "Elasticity of Fluid-Infiltrated Porous Solids (Poroelasticity)"
  #  this constant comes from the stress-strain constitutive relationship, 
  #  under the following situation:
  # Suppose that all pore space is fluid infiltrated, and that all the 
  # solid phase consists of material elements which respond isotropically 
  # to pure pressure stress states, with the same bulk modulus Ks . 
  # Suppose we simultaneously apply a pore pressure p = po and 
  # macroscopic stresses amounting to compression by po on all faces 
  # (sig11 = 22 = 33 = -po ). That results in a local stress state 
  # of po_ij*kron_ij at each point of the solid phase. So each point 
  # of the solid phase undergoes the strain po_ij*kron_ij / 3 Ks , which 
  # means that all linear dimensions of the material, including those 
  # characterizing void size, reduce by the (very small) fractional amount 
  # po / 3Ks , causing the macroscopic strains, and change in porosity, 
  # Eps_11 = 22 = 33 = -po / 3 Ks and del_n/n = -po/Ks
  #
  # R A 89, eq 2, where Beta = 1/K and Beta_u = Ks in the previous description
  stopifnot(Beta_u <= Beta)
  alph <- 1 - Beta_u / Beta
  stopifnot((alph>=0)&(alph<=1)) # redundant?
  return(alph)
}
