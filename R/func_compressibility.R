#' Calculate undrained compressibility
#' 
#' Calculates Equation (6) from Rojstaczer and Agnew (1989): \eqn{\hat{Beta}}
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
#' B assumesthat the rock matrix is homogeneous and all the 
#' pore space is interconnected.
#' 
#' @references S. Rojstaczer and D.C. Agnew (1989), 
#' “The Influence of Formation Material Properties on the Response of Water Levels in Wells to Earth Tides and Atmospheric Loading,” 
#' \emph{J. Geophys. Res.}, \strong{94} (B9), pp. 12403-12411.
#'
#
undrained_compressibility.beta <- function(Beta, B., 
                                           Beta_u=2e-11){
  alph <- .calc_alpha(Beta, Beta_u)
  Beta_hat <- Beta * (1 - B. * alph)
  return(Beta_hat)
}
#
undrained_compressibility.from.areal_strain_sens <- function(As., B.,
                                             nu_u=0.25, 
                                             fluid_dens=1000){
  prat <- .calc_poissrat(B., nu_u, fluid_dens)
  Beta_hat <- prat / As.
  return(Beta_hat)
}
#
areal_strain_sens.from.undrained_compressibility <- function(Beta_hat, B.,
                                             nu_u=0.25, 
                                             fluid_dens=1000){
  prat <- .calc_poissrat(B., nu_u, fluid_dens)
  As. <- prat / Beta_hat
  return(As.)
}
#
.calc_poissrat <- function(B., nu_u, fluid_dens){
  stopifnot(nu_u>=0 & nu_u<=1)
  stopifnot(B.>=0 & B.<=1)
  mnu <- 1 - nu_u
  prat <- (mnu - nu_u) * B. / fluid_dens / 9.81 / mnu
  return(prat)
}
#
.calc_alpha <- function(Beta, 
                        Beta_u=2e-11){
  alph <- 1 - Beta/Beta_u
  return(alph)
}
#