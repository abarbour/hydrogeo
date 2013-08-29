\docType{data}
\name{hydrogeo-constants}
\alias{constants}
\alias{hydrogeo-constants}
\title{Constants used as defaults}
\format{List of 6
 $ gravity        :List of 1
 $ water          :List of 2
 $ Poisson        :List of 3
 $ compressibility:List of 2
 $ atm            :List of 6
 $ conversions    :List of 1}
\usage{
  .constants

  constants(do.str = TRUE)
}
\arguments{
  \item{do.str}{logical; should the structure be printed?}
}
\description{
  The hydrogeologic response of an aquifer system depends
  on its mechanical and hydraulic properties; if these are
  not known or specified, these constants are used.
}
\details{
  The helper function \code{\link{constants}} shows (the
  structure of, optionally) and returns \code{.constants}.

  The following constants are assumed \describe{
  \item{gravity}{} \item{pore-fluid}{ (water)}
  \item{Poisson's ratios}{ (Poisson solid)}
  \item{compressibility}{ } \item{atmosphere}{ STP} }
}
\seealso{
  \code{\link{hydrogeo-units}}, \code{\link{hydrogeo}}
}
\keyword{datasets}

