\name{sand_silt_clay}
\alias{sand_silt_clay}
\alias{shepard_plot}
\title{Plot a ternary diagram of sand-silt-clay grain-size percentage.}
\usage{
  sand_silt_clay(shepard.diagram = TRUE,
    silt.lims = c(-0.01, 1.01), add.frame = FALSE, ...)

  shepard_plot()
}
\arguments{
  \item{shepard.diagram}{logical; should Shepards' (1954)
  classification system be overlain?}

  \item{silt.lims}{numeric; vector of length two
  controlling the horizontal (silt) axis limits.}

  \item{add.frame}{logical; should a box be drawn around
  it?}
}
\value{
  \code{NULL}
}
\description{
  The hydraulic properties of sedimentary material are
  roughly dependent on the proportions of grain size due to
  sand, silt, and clay; these are common categorical
  distinctions found in drillers' logs.
}
\details{
  A more detailed classification system for soils is used
  by the USDA; however, Shepard's diagram is adquate for a
  sand-silt-clay model, and \code{shepard_plot} adds them
  to the current device. This function is intended for a
  ternary diagram normalized to percent particle size so
  that hydraulic parameters may be estimated from drillers'
  logs.
}
\author{
  Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
  \code{\link{xyz2ternary}}, \code{\link{hydrogeo}}
}

