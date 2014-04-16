\name{compressibility}
\alias{.calc_prat}
\alias{areal_strain_sens.from.undrained_compressibility}
\alias{calc_alpha}
\alias{calc_nu_u}
\alias{compressibility}
\alias{undrained_compressibility.from.areal_strain_sens}
\alias{undrained_compressibility.from.beta}
\title{Calculate compressibility and strain sensitivity.}
\usage{
undrained_compressibility.from.beta(Beta, B., Beta_u = NULL)

undrained_compressibility.from.areal_strain_sens(As., B., ...)

areal_strain_sens.from.undrained_compressibility(Beta_hat, B., ...)

.calc_prat(B., nu_u = NULL, fluid_dens = NULL)

calc_alpha(Beta, Beta_u = NULL)

calc_nu_u(B., Beta, nu = NULL, ...)
}
\arguments{
  \item{As.}{numeric; areal strain sensitivity \eqn{A[s]}}

  \item{B.}{numeric; Skempton's coefficient \eqn{B}}

  \item{Beta}{numeric; The compressibility of the solid
  matrix, \eqn{\beta}}

  \item{Beta_hat}{numeric; \eqn{\hat{\beta}}}

  \item{Beta_u}{numeric; \eqn{\beta} for an undrained
  state}

  \item{fluid_dens}{numeric; the density \eqn{\rho} of the
  fluid in consideration}

  \item{nu}{numeric; Poisson's ratio \eqn{\nu}}

  \item{nu_u}{numeric; undrained Poisson's ratio \eqn{\nu}}

  \item{...}{additional paramters passed from \itemize{
  \item{\code{undrained_compressibility.from.areal_strain_sens}
  and
  \code{areal_strain_sens.from.undrained_compressibility}}{
  to \code{.calc_prat}} \item{\code{calc_nu_u}}{ to
  \code{calc_alpha}} }}
}
\description{
Compressibility is, essentially, the inverse of the bulk
modulus of a medium, and the strain sensitivity is the
amount of strain produced for unit increments in pressure.
}
\details{
\code{X} calculates Equation (6) from Rojstaczer and Agnew
(1989): \eqn{\hat{\beta}} and Equation (10) using strain
sensitivity
}
\section{Assumptions}{
  The intake of the well is assumed to penetrate a porous
  elastic medium with uniform properties. These properties
  are those specified by the theory of Biot [1941] (as
  reexpressed by Rice and Cleary [1976] and Green and Wang
  [1986]), namely the compressibilities of the solid phase
  \eqn{\beta[u]}, the fluid phase \eqn{\beta[f]}, and the
  porous matrix when drained of fluid Beta (the matrix
  compressibility), together with the Poisson's ratio
  \eqn{\nu}, of the matrix, the porosity \eqn{\phi}, and
  the permeability \eqn{\kappa}.

  The fluid phase is assumed to be water at standard
  temperature and pressure; this can be severely violated
  deep in the earth.
}

\section{Physical parameters}{
  \subsection{\eqn{\nu[u]}}{ \code{calc_nu_u} can be used
  to calculate \eqn{\nu[u]} } \subsection{\eqn{\alpha}}{

  Rice (1998, "Elasticity of Fluid-Infiltrated Porous
  Solids (Poroelasticity)") notes:

  [...] this constant comes from the stress-strain
  constitutive relationship, under the following situation:
  Suppose that all pore space is fluid infiltrated, and
  that all the solid phase consists of material elements
  which respond isotropically to pure pressure stress
  states, with the same bulk modulus Ks . Suppose we
  simultaneously apply a pore pressure \eqn{p = p[o]} and
  macroscopic stresses amounting to compression by po on
  all faces \eqn{\sigma[11] = \sigma[22] = \sigma[33] =
  -p[o]}. That results in a local stress state of
  \eqn{p[o,ij] \delta[ij]} at each point of the solid
  phase. So each point of the solid phase undergoes the
  strain \eqn{p[o,ij] \delta[ij] / 3 K[s]} , which means
  that all linear dimensions of the material, including
  those characterizing void size, reduce by the (very
  small) fractional amount \eqn{p[o] / 3 K[s]} , causing
  the macroscopic strains, and change in porosity,
  \eqn{\epsilon[11] = \epsilon[22] = \epsilon[33] = -p[o] /
  3 K[s]} and \eqn{\Delta[n]/n = -p[o] / K[s]}

  \code{calc_alpha} can be used to calculate \eqn{\alpha} }
}
\author{
Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
\code{\link{skempton}}, \code{\link{kinvisc}},
\code{\link{hydrogeo.p}}
}

