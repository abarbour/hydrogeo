#' Elevation pressure and head.
#' 
#' A collection of functions to deal with atmospheric pressure and head
#' calculations.
#' 
#' \code{\link{atmos_p}} returns atmospheric pressure in the units of meters or feet.
#' 
#' \code{\link{atmos_head}} calculates the amount of head of water produced by 
#' atmospheric pressure for a given elevation.
#' 
#' \code{\link{m_per_hpa}} calculates the expected scaling between water level (head) 
#' and pressure for a given elevation.
#' 
#' \code{\link{hpa2m}} calculates the water level (head) for a given pressure and elevation.
#' 
#' \code{\link{elev2hpa}} calculates the expected RMS tropospheric pressure head, 
#' by correcting sea level at STP for lapse effects.
#'
#' @name hydrogeo.p-atmosphere
#' @rdname hydrogeo.p-atmosphere
#' @param pressure.units character; the units of atmospheric pressure
#' @param hpa numeric; pressure in hecto-Pascals
#' @param elev numeric; the elevation
#' @param elev.m numeric; the elevation, in meters
#' @param elev.units character; the units of elevation associated with \code{elev}
#' @return numeric
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{hydrogeo.p-units}}, \code{\link{hydrogeo.p}}
NULL

#' @rdname hydrogeo.p-atmosphere
#' @export
atmos_p <- function(pressure.units=c("hpa","bar")){
	punits <- match.arg(pressure.units)
  const <- get_constants()
	atm.bar <- const$atm$bar
	atmp <- switch(punits, hpa=atm.bar*pa2hpa(bar2pa(1)), bar=atm.bar)
	return(atmp)
}

#' @rdname hydrogeo.p-atmosphere
#' @export
atmos_head <- function(elev.m=0){
  # 10.3 m/atm (m/1013.25hPa)
  const <- get_constants()
  atm.m <- const$atm$m_per
  return(atm.m * elev2hpa(elev.m, elev.units="m")/elev2hpa(0, elev.units="m"))
}

#' @rdname hydrogeo.p-atmosphere
#' @export
m_per_hpa <- function(elev.m=0){
	ppa <- elev2hpa(elev.m) # atmosphere at elev.m
	pm <- atmos_head(0) # meters per atmosphere, at elev.m
	return(pm/ppa)
}

#' @rdname hydrogeo.p-atmosphere
#' @export
hpa2m <- function(hpa=0, elev.m=0){
	sc <- m_per_hpa(elev.m)
	return(sc*hpa)
}

#' @rdname hydrogeo.p-atmosphere
#' @references \url{http://en.wikipedia.org/wiki/Atmospheric_pressure}
#' @export
elev2hpa <- function(elev=0, elev.units=c("m","ft")){
  # get elevation in meters
  units <- match.arg(elev.units)
  elev.m <- switch(units, ft=ft2m(elev), m=elev)
  # see http://en.wikipedia.org/wiki/Atmospheric_pressure
  #p0	sea level standard atmospheric pressure	101325 Pa
  #L	temperature lapse rate	0.0065 K/m
  #T0	sea level standard temperature	288.15 K
  #g	Earth-surface gravitational acceleration	9.80665 m/s2
  #M	molar mass of dry air	0.0289644 kg/mol
  #R	universal gas constant	8.31447 J/(mol*K)
  Po. <- atmos_p() #1013.25
  const <- get_constants()
  grav <- const$gravity$std
  atm <- const$atm
  L. <- atm$L.
  To. <- atm$To.
  M. <- atm$M.
  R. <- atm$R.
  Cexp <- grav * M. / R. / L.
  toret <- Po. * {1 - L. * elev.m / To.} ** Cexp
  if (NA %in% (toret)){
    warning("elevation pressure calc. reached (and returned) machine precision")
    return(.Machine$double.eps)
  } else {
    return(toret)
  }
}
