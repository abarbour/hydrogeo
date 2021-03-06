% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_transmissivity.R
\name{transmissivity}
\alias{effective_aquifer_thickness}
\alias{transmissivity}
\title{Aquifer transmissivity.}
\usage{
transmissivity(Storativ, Diffusiv)

effective_aquifer_thickness(Storativ, Diffusiv, Conductiv)
}
\arguments{
\item{Storativ}{numeric; the storativity, with units \eqn{[X]}}

\item{Diffusiv}{numeric; the diffusivity, with units \eqn{[X]}}

\item{Conductiv}{numeric; the hydraulic conductivity, with units \eqn{[X]}}
}
\value{
numeric
}
\description{
A collection of functions used to estimate the transmissivity of
 an idealized aquifer system, or the effective length scale (thickness)
 of an aquifer.
}
\details{
\code{\link{transmissivity}} calculates the thickness, \eqn{[m]}
}
\author{
Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
\code{\link{hydraulic_diffusivity}}, \code{\link{hydrogeo.p-constants}},
\code{\link{hydrogeo.p}}
}

