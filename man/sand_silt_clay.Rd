\name{sand_silt_clay}
\alias{plot.shepard}
\alias{sand_silt_clay}
\title{Plot a ternary diagram Sand-Silt-Clay grain-size data.}
\usage{
  sand_silt_clay(shepard.diagram = TRUE,
    silt.lims = c(-0.01, 1.01), add.frame = FALSE, ...)

  plot.shepard()
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
\author{
  Andrew J. Barbour
}
\seealso{
  \code{\link{xyz2ternary}}
}

