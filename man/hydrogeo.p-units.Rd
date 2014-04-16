\name{hydrogeo.p-units}
\alias{atm2bar}
\alias{bar2pa}
\alias{ft2m}
\alias{hpa2pa}
\alias{hydrogeo.p-units}
\alias{kpa2pa}
\alias{m2ft}
\alias{pa2bar}
\alias{pa2hpa}
\alias{pa2kpa}
\alias{to_cmsquared}
\alias{to_darcies}
\alias{to_millidarcies}
\alias{to_msquared}
\title{Conversion of units.}
\usage{
to_msquared(Perm_Darcy)

to_darcies(Perm_sqm, milli = FALSE)

to_millidarcies(Perm_sqm)

to_cmsquared(Perm_sqm)

ft2m(ft)

m2ft(m)

hpa2pa(hpa)

pa2hpa(pa)

kpa2pa(kpa)

pa2kpa(pa)

bar2pa(bar)

pa2bar(pa)

atm2bar(atm)
}
\arguments{
  \item{atm}{numeric; pressure, in standard atmospheres}

  \item{bar}{numeric; pressure in bars}

  \item{ft}{numeric; length in feet}

  \item{hpa}{numeric; pressure in hecto-Pascals}

  \item{kpa}{numeric; pressure in kilo-Pascals}

  \item{m}{numeric; length in meters}

  \item{pa}{numeric; pressure in Pascals}

  \item{Perm_sqm}{numeric; permeability, in \eqn{[m^2]}}

  \item{Perm_Darcy}{numeric; permeability, in darcies}

  \item{milli}{logical; should the result be divided by
  1000?}
}
\description{
Convert within commonly found units.
}
\details{
There is a panoply of units in hydrogeology literature, so
this represents a very small fraction of possibilities.
There are generally three sets of conversion tools here:

\itemize{ \item{length}{ convert meters to feet, etc}
\item{permeability}{ convert from darcies to SI, etc; these
will begin with \code{to_}. For example, \code{to_msquared}
converts darcies to \eqn{m^2}.} \item{pressure}{ convert
from pascal to bars, etc} }
}
\author{
Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
\code{\link{dimensional_units}}, \code{\link{hydrogeo.p}}
}

