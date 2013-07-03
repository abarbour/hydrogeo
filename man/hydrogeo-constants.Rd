\docType{data}
\name{hydrogeo-constants}
\alias{constants}
\alias{hydrogeo-constants}
\title{Constants used in many calculations.}
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
  Constants used in many calculations.
}
\details{
  The helper function \code{\link{constants}} shows (the
  structure of, optionally) and returns \code{.constants}.
}
\section{Typical Values}{
  \describe{ \item{Hydraulic conductivity,
  \eqn{C}}{\eqn{10^{-0} - 10^{-0}}} }
}
\seealso{
  \code{\link{hydrogeo-units}}, \code{\link{hydrogeo}}
}
\keyword{datasets}

