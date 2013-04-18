\name{hydrogeo-units}
\alias{hydrogeo-units}
\alias{to_bars}
\alias{to_cmsquared}
\alias{to_darcies}
\alias{to_millidarcies}
\alias{to_msquared}
\title{Conversion of units.}
\usage{
  to_bars(Pressure_atm)

  to_msquared(Perm_Darcy)

  to_darcies(Perm_sqm, milli = FALSE)

  to_millidarcies(Perm_sqm)

  to_cmsquared(Perm_sqm)
}
\arguments{
  \item{Pressure_atm}{numeric; pressure, in standard
  atmospheres}

  \item{Perm_sqm}{numeric; permeability, in \eqn{[m^2]}}

  \item{Perm_Darcy}{numeric; permeability, in darcies}
}
\description{
  Convert within commonly found units.
}
\details{
  There is a panoply of units in hydrogeology literature,
  so this represents a very small fraction of
  possibilities.
}
\author{
  Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
  \code{\link{dimensional_units}}, \code{\link{hydrogeo}}
}

