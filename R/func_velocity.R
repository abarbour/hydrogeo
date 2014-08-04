#' Convert velocity or slowness to km/s.
#' 
#' Converts a velocity with units ft/s, or slowness with units s/ft
#' to velocity with units km/s. This function would typically be
#' needed to convert units in \eqn{V[p]} logs to those appropriate
#' for scaling laws (into density, porosity, etc.).
#' 
#' If a velocity is \eqn{v} then slowness is \deqn{p=\frac{1}{v}}
#' 
#' The spatial units associated with \code{vel} are assumed to be in
#' feet (e.g. ft/s).
#' 
#' @aliases to_kms
#' @param vel numeric; velocity or slowness
#' @param is.mus logical; is time in microseconds?
#' @param is.slowness logical; is \code{vel} slowness instead of velocity?
#' @param ... additional parameters (unused)
#' @return numeric
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{brocher}}, \code{\link{hydrogeo.p}}
kms <- function(vel, is.mus=FALSE, is.slowness=FALSE, ...) UseMethod("kms")

#' @rdname kms
#' @export
kms.default <- function(vel, is.mus=FALSE, is.slowness=FALSE, ...){
  # ensure velocity if slowness (p=1/vel)
  if (is.slowness) vel <- 1 / vel
  # ensure time is in seconds
  if (is.mus) vel <- vel * 1e6
  # km per sec
  return(vel / (3280 + (10 + 5/64)/12))
}