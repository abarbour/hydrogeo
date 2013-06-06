\name{brocher}
\alias{as.PoissonsRatio}
\alias{as.Porosity}
\alias{as.RockDensity}
\alias{as.Vp}
\alias{as.Vs}
\alias{brocher}
\alias{default}
\alias{dens_christensen_mooney}
\alias{dens_christensen_mooney.default}
\alias{dens_gardner}
\alias{dens_gardner.default}
\alias{dens_godfrey}
\alias{dens_godfrey.default}
\alias{dens_nafe_drake}
\alias{dens_nafe_drake.default}
\alias{From.Vp}
\alias{nu_brocher}
\alias{nu_brocher_ludwig}
\alias{nu_brocher_ludwig.default}
\alias{nu_brocher.default}
\alias{nu_continuum}
\alias{nu_continuum.default}
\alias{vp_brocher}
\alias{vp_brocher_ludwig}
\alias{vp_brocher_ludwig.default}
\alias{vp_brocher.default}
\alias{vpvs_continuum}
\alias{vpvs_continuum.default}
\alias{vs_brocher}
\alias{vs_brocher_mafic}
\alias{vs_brocher_mafic.default}
\alias{vs_brocher.default}
\alias{vs_castagna}
\alias{vs_castagna.default}
\title{Empirical scalings to convert between material properties and seismic velocities.}
\usage{
  From.Vp(Vp, ...)

  as.RockDensity(Vp, pos = seq_along(Vp),
    emp.fit = c("nafe_drake", "gardner", "christensen_mooney", "godfrey"),
    do.all = TRUE, all.average = do.all, Vp.units = NULL,
    verbose = TRUE)

  dens_nafe_drake(Vp, null_val = NA)

  \method{dens_nafe_drake}{default} (Vp, null_val = NA)

  dens_gardner(Vp, null_val = NA)

  \method{dens_gardner}{default} (Vp, null_val = NA)

  dens_christensen_mooney(Vp, null_val = NA)

  \method{dens_christensen_mooney}{default} (Vp,
    null_val = NA)

  dens_godfrey(Vp, null_val = NA)

  \method{dens_godfrey}{default} (Vp, null_val = NA)

  as.Vs(Vp, pos = seq_along(Vp),
    emp.fit = c("brocher", "castagna", "brocher_mafic"),
    do.all = TRUE, all.average = do.all, Vp.units = NULL,
    verbose = TRUE)

  vs_brocher(Vp, null_val = NA)

  \method{vs_brocher}{default} (Vp, null_val = NA)

  vs_castagna(Vp, null_val = NA)

  \method{vs_castagna}{default} (Vp, null_val = NA)

  vs_brocher_mafic(Vp, null_val = NA)

  \method{vs_brocher_mafic}{default} (Vp, null_val = NA)

  as.Vp(X, pos = seq_along(X),
    emp.fit = c("brocher", "brocher_ludwig"),
    do.all = FALSE, all.average = FALSE, Vp.units = NULL,
    verbose = TRUE)

  vp_brocher(Vs, null_val = NA)

  \method{vp_brocher}{default} (Vs, null_val = NA)

  vp_brocher_ludwig(Dens.bulk, null_val = NA)

  \method{vp_brocher_ludwig}{default} (Dens.bulk,
    null_val = NA)

  vpvs_continuum(nu)

  \method{vpvs_continuum}{default} (nu)

  nu_continuum(Vp, Vs)

  \method{nu_continuum}{default} (Vp, Vs)

  as.PoissonsRatio(Vp, pos = seq_along(Vp),
    emp.fit = c("brocher", "brocher_ludwig"),
    do.all = FALSE, all.average = FALSE, Vp.units = NULL,
    verbose = TRUE)

  nu_brocher(Vp, null_val = NA)

  \method{nu_brocher}{default} (Vp, null_val = NA)

  nu_brocher_ludwig(Vp, null_val = NA)

  \method{nu_brocher_ludwig}{default} (Vp, null_val = NA)

  as.Porosity(Dens.bulk, Dens.solid = 2.8, Dens.fluid = 1,
    non.neg = TRUE, dens.units = NULL,
    return.percent = FALSE, return.voidfrac = FALSE,
    verbose = TRUE)
}
\arguments{
  \item{X}{numeric; An object to convert}

  \item{nu}{numeric; Poisson's ratio}

  \item{Dens.bulk}{numeric;}

  \item{Dens.solid}{numeric;}

  \item{Dens.fluid}{numeric;}

  \item{non.neg}{logical;}

  \item{dens.units}{character;}

  \item{return.percent}{logical;}

  \item{return.voidfrac}{logical;}

  \item{verbose}{logical;}

  \item{Vp}{numeric; P-wave velocity in km/s}

  \item{Vs}{numeric; S-wave velocity in km/s}

  \item{Vp.units}{character; the units of \code{Vp}}

  \item{pos}{numeric; vector of indices or distance for Vp}

  \item{emp.fit}{character; the empirical fit to use
  [overridden if \code{(do.all | all.average)}]}

  \item{do.all}{logical; should all available epirical
  scalings be calculated?}

  \item{all.average}{logical; should, once all available
  epirical scalings are calculated, the collection of
  results be averaged?}

  \item{null_val}{the}

  \item{...}{additional parameters}
}
\value{
  A \code{data.frame} for the \code{as.} functions; numeric
  otherwise.
}
\description{
  Uses the (empirical) relations from Brocher (2005) to
  convert material properties into seismic velocities, and
  the other way around. A function to convert density into
  porosity is also provided.
}
\details{
  The empirical relations are based on regression curves,
  and thus restricted to within specific ranges of the
  independent variable; the specific ranges are:

  \tabular{llcr}{ \emph{Valid rock/lithology types} \tab
  \emph{Function} \tab \emph{Validity range} \tab
  \emph{Brocher 2005 eqn.}\cr \strong{\eqn{V[p]} to
  \eqn{\rho} conversions} \tab \tab \strong{km/s} \tab \cr
  All except mafic crustal and CA-rich\tab
  \code{dens_nafe_drake} \tab \eqn{1.5--8.0}\tab 1\cr
  Sedimentary \tab \code{dens_gardner} \tab
  \eqn{1.5--6.1}\tab 2\cr Cyrstalline (except volcanic and
  monomineralic) \tab \code{dens_christensen_mooney} \tab
  \eqn{5.5--7.5}\tab 3\cr Basalt, diabase, and gabbro \tab
  \code{dens_godfrey} \tab \eqn{5.9--7.1}\tab 4\cr
  \strong{\eqn{\rho} to \eqn{V[p]} conversions} \tab \tab
  \strong{g/cm^3} \tab \cr (see \code{dens_nafe_drake})
  \tab \code{vp_brocher_ludwig} \tab \eqn{2.0--3.5} \tab
  5\cr \strong{\eqn{V[p]} to \eqn{V[s]} conversions} \tab
  \tab \strong{km/s} \tab \cr All except CA-rich and mafic,
  gabbros, and serpentinites \tab \code{vs_brocher} \tab
  \eqn{1.5--8.0} \tab 6\cr Clay-rich sedimentary \tab
  \code{vs_castagna} \tab \eqn{1.5--4.25} \tab 7\cr CA-rich
  (inc. dolomites and anorthosites), mafic, and gabbros
  \tab \code{vs_brocher_mafic} \tab \eqn{5.25--7.25} \tab
  8\cr \strong{\eqn{V[s]} to \eqn{V[p]} conversions} \tab
  \tab \strong{km/s} \tab \cr (See \code{vs_brocher}) \tab
  \code{vp_brocher} \tab \eqn{0.0--4.5} \tab 9\cr
  \strong{\eqn{V[p]} to \eqn{\nu} conversions} \tab \tab
  \strong{km/s} \tab \cr (See \code{vs_brocher}) \tab
  \code{nu_brocher} \tab \eqn{1.5--8.5} \tab 11\cr (See
  \code{vs_brocher}) \tab \code{nu_brocher_ludwig} \tab
  \eqn{1.5--8.5} \tab 12\cr }

  The functions \code{nu_continuum} and
  \code{vpvs_continuum} return Poisson's ratio and the
  \eqn{V[p]/V[s]} ratio for a continuum representation; is
  missing arguments they default to a Poisson solid
  (\eqn{\nu=1/4} or \eqn{V[p]/V[s] = \sqrt{3}}).

  The parameter \code{null_val} can be set to fill points
  where any conversion lies outside the given range.
}
\examples{
From.Vp(rnorm(5)+4)
as.Vs(rnorm(5)+4)
}
\author{
  Andrew J. Barbour <andy.barbour@gmail.com>
}
\seealso{
  \code{\link{kms}}, \code{\link{hydrogeo}}
}

