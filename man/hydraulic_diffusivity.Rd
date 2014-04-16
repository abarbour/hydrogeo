\name{hydraulic_diffusivity}
\alias{diffusivity_length}
\alias{diffusivity_time}
\alias{hydraulic_diffusivity}
\alias{hydraulic_diffusivity_2}
\alias{hydraulic_diffusivity_poro}
\title{Hydraulic diffusivity.}
\usage{
hydraulic_diffusivity(Transmiss, Storativ)

hydraulic_diffusivity_2(Conductiv, SpecificStorage)

hydraulic_diffusivity_poro(Permeab, B., Beta, nu_u = NULL, nu = NULL)

diffusivity_length(Diffusiv, Time.s = 1)

diffusivity_time(Diffusiv, Length.m = 1)
}
\arguments{
  \item{Conductiv}{numeric; the hydraulic conductivity,
  with units \eqn{[X]}}

  \item{Diffusiv}{numeric; the diffusivity, with units
  \eqn{m^2/s}}

  \item{Permeab}{numeric; the permeability, with units
  \eqn{m^2}}

  \item{Storativ}{numeric; the storativity, with units
  \eqn{[X]}}

  \item{SpecificStorage}{numeric; the specific storage,
  with units \eqn{[X]}}

  \item{Transmiss}{numeric; the transmissivity, with units
  \eqn{[X]}}

  \item{B.}{numeric; Skempton's coefficient \eqn{B}}

  \item{Beta}{numeric; the bulk compressibility}

  \item{nu_u}{numeric; undrained Poisson's ratio}

  \item{nu}{numeric; Poisson's ratio}

  \item{Time.s}{numeric; the time scale, in \eqn{[s]}, to
  calcuate characteristic diffusion length for.}

  \item{Length.m}{numeric; the length scale, in \eqn{[m]},
  to calcuate characteristic diffusion time for.}
}
\value{
numeric
}
\description{
A collection of various functions used to estimate
hydraulic diffusivity, or characteristic length scales.
}
\details{
Because diffusivity is scale dependent, there exists a
number of definitions, including

\describe{
\item{\code{\link{hydraulic_diffusivity}}}{following Eq
(1)} \item{\code{\link{hydraulic_diffusivity_2}}}{following
Eq (2)}
\item{\code{\link{hydraulic_diffusivity_poro}}}{following
Eq (3)} \item{\code{\link{diffusivity_length}}}{following
Eq (4)} \item{\code{\link{diffusivity_time}}}{following Eq
(5)} }
}
\author{
Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
\code{\link{hydrogeo.p}},
\code{\link{hydraulic_conductivity}},
\code{\link{transmissivity}}, \code{\link{skempton}},
\code{\link{storativity}}
}

