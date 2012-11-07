\name{dimensional_units}
\alias{dimensional_units}
\title{Gives dimensional units for quantity desired.

Poroelasticity and hydrogeology can be a mish-mash of units;
keeping them straight can be challenging.  This returns a
string with the dimensional units, and (optionally) prints them
as a message.

The dimensional representations this far are
\describe{
\item{L}{length}
\item{T}{time}
\item{M}{mass}
}}
\usage{
  dimensional_units(quantity = c("conductivity", "diffusivity", "transmissivity", "storativity", "specific.storage", "viscosity", "density", "kinematic.viscosity", "permeability", "force", "stress", "strain", "compressibility", "porosity", "skempton"),
    verbose = TRUE)
}
\arguments{
  \item{quantity}{char, representing the quantity to show
  units for}

  \item{verbose}{boolean, should a message be printed
  also?}
}
\value{
  invisible char tuple with quantity and quantity-units
}
\description{
  Gives dimensional units for quantity desired.

  Poroelasticity and hydrogeology can be a mish-mash of
  units; keeping them straight can be challenging.  This
  returns a string with the dimensional units, and
  (optionally) prints them as a message.

  The dimensional representations this far are \describe{
  \item{L}{length} \item{T}{time} \item{M}{mass} }
}
\examples{
### code to be run
dimensional_units("diff") #matches 'diffusivity'
print(dimensional_units("diff", FALSE))
}
\author{
  Andrew Barbour <andy.barbour@gmail.com>
}
\seealso{
  \code{\link{millidarcies}}, \code{\link{cmsquared}}
}

