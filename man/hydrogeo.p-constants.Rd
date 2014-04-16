\docType{data}
\name{hydrogeo.p-constants}
\alias{constants}
\alias{get_constants}
\alias{hydrogeo.p-constants}
\title{Constants used as defaults}
\format{\preformatted{List of 6
 $ gravity        :List of 1
  ..$ std: num 9.81
 $ water          :List of 2
  ..$ dens    : num 1000
  ..$ dyn_visc: num 0.001
 $ Poisson        :List of 3
  ..$ nu  : num 0.25
  ..$ nu_u: num 0.333
  ..$ VpVs: num 1.73
 $ compressibility:List of 2
  ..$ Beta_u: num 2e-11
  ..$ Beta_f: num 4.4e-10
 $ atm            :List of 6
  ..$ bar  : num 1.01
  ..$ m_per: num 10.3
  ..$ L.   : num 0.0065
  ..$ To.  : num 288
  ..$ M.   : num 0.029
  ..$ R.   : num 8.31
 $ conversions    :List of 1
  ..$ sqm2sqcm: num 10000
}}
\usage{
.hg_constants

constants(do.str = TRUE)

get_constants()
}
\arguments{
  \item{do.str}{logical; should the structure be printed?}
}
\description{
The hydrogeologic response of an aquifer system depends on
its mechanical and hydraulic properties; if these are not
known or specified, these constants are used.
}
\details{
The helper function \code{\link{constants}} shows (the
structure of, optionally) and returns \code{.hg_constants}.
\code{\link{get_constants}} simply accesses
\code{\link{constants}}

The following constants are assumed \describe{
\item{gravity}{} \item{pore-fluid}{ (water)}
\item{Poisson's ratios}{ (Poisson solid)}
\item{compressibility}{ } \item{atmosphere}{ STP} }
}
\seealso{
\code{\link{hydrogeo.p-units}}, \code{\link{hydrogeo.p}}
}
\keyword{datasets}

