\name{kinvisc}
\alias{kinvisc}
\title{Calculate the kinematic viscosity of a fluid.}
\usage{
  kinvisc(dvisc = 0.001002, fdens = 1000,
    grav.divide = FALSE, verbose = FALSE)
}
\arguments{
  \item{dvisc}{scalar; representing the dynamic fluid
  viscosity, with units \eqn{[Pa \cdot s]}}

  \item{fdens}{scalar; representing the fluid density, with
  units \eqn{[kg m^{-3}]}}

  \item{grav.divide}{logical; should the kinematic viscosiy
  be divided by standard gravitational acceleration?}

  \item{verbose}{logical; should messages be displayed?}
}
\value{
  scalar, a form of kinematic viscosity with units or
  either: \itemize{ \item{\eqn{[m^2 s^{-1}]} if
  \code{grav.divide==FALSE}} \item{\eqn{[m s]} if
  \code{grav.divide==TRUE}} }
}
\description{
  The kinematic viscosity of a homogeneous body or fluid is
  defined as the value of its dynamic viscosity divided by
  its density.  This function can also divide by gravity,
  which would be necessary in the computation of, for
  example, hydraulic conductivity.
}
\examples{
kinvisc()	# use defaults (water at STP)
kinvisc(verbose=TRUE)	# no message
kinvisc(grav.divide=TRUE)	# divide by standard gravity
kinvisc(grav.divide=TRUE, verbose=TRUE)	# message
}
\author{
  Andrew Barbour <andy.barbour@gmail.com>
}
\seealso{
  \code{\link{hydrogeo}},
  \code{\link{hydraulic_conductivity}}
}

