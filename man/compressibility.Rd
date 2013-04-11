\name{compressibility}
\alias{.calc_alpha}
\alias{.calc_nu_u}
\alias{.calc_prat}
\alias{areal_strain_sens.from.undrained_compressibility}
\alias{compressibility}
\alias{undrained_compressibility.from.areal_strain_sens}
\alias{undrained_compressibility.from.beta}
\title{Calculate compressibilities and strain sensitivity.}
\usage{
  undrained_compressibility.from.beta(Beta, B.,
    Beta_u = 2e-10)

  undrained_compressibility.from.areal_strain_sens(As., B.,
    ...)

  areal_strain_sens.from.undrained_compressibility(Beta_hat,
    B., ...)

  .calc_prat(B., nu_u = 0.35, fluid_dens = 1000)

  .calc_nu_u(B., Beta, nu = 0.25, ...)

  .calc_alpha(Beta, Beta_u = 2e-11)
}
\arguments{
  \item{Beta}{numeric; The compressibility of the solid
  matrix, \eqn{\beta}}

  \item{B.}{numeric; Skempton's coefficient \eqn{B}}

  \item{Beta_u}{numeric; \eqn{\beta} for an undrained
  state}

  \item{As.}{numeric; areal strain sensitivity \eqn{A_s}}

  \item{nu_u}{numeric; undrained Poisson's ratio \eqn{\nu}}

  \item{fluid_dens}{numeric; the density \eqn{\rho} of the
  fluid in consideration}

  \item{...}{\code{\link{.from.areal_strain_sens}}:
  additional parameters passed to \code{\link{.calc_prat}}}

  \item{Beta_hat}{numeric; \eqn{\hat{\beta}}}

  \item{...}{\code{\link{.from.undrained_compressibility}}:
  additional parameters passed to \code{\link{.calc_prat}}}

  \item{nu}{numeric; Poisson's ratio \eqn{\nu}}

  \item{...}{\code{\link{.calc_nu_u}}: additional
  parameters passed to \code{\link{.calc_alpha}}}
}
\description{
  Calculates Equation (6) from Rojstaczer and Agnew (1989):
  \eqn{\hat{\beta}} and Equation (10) using strain
  sensitivity
}
\details{
  Assumptions: The intake of the well is assumed to
  penetrate a porous elastic medium with uniform
  properties. These properties are those specified by the
  theory of Biot [1941] (as reexpressed by Rice and Cleary
  [1976] and Green and Wang [1986]), namely the
  compressibilities of the solid phase Beta_u, the fluid
  phase Beta_f, and the porous matrix when drained of fluid
  Beta (the matrix compressibility), together with the
  Poisson's ratio nu, of the matrix, the porosity phi, and
  the permeability kappa.

  As noted by Rice and Cleary [1976], this definition for B
  assumes that the rock matrix is homogeneous and all the
  pore space is interconnected.
}
\references{
  S. Rojstaczer and D.C. Agnew (1989), “The Influence of
  Formation Material Properties on the Response of Water
  Levels in Wells to Earth Tides and Atmospheric Loading,”
  \emph{J. Geophys. Res.}, \strong{94} (B9), pp.
  12403-12411.
}

