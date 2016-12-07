#http://www.ees.nmt.edu/outside/courses/GEOP523/Docs/modtable.pdf

#' Elastic properties from seismic velocities
#' @name Elastic.Properties
#' @export
#' @aliases elasticProperties elastic_properties
#' @param Vp numeric; the P-wave velocity of the rock in km/s
#' @param Vs numeric; the S-wave velocity of the rock in km/s
#' @param dens numeric; the rock density, in kg/m^3
#' @param mu numeric; the shear modulus of the rock, in Pa
Elastic.Properties <- function(Vp, Vs, dens){
  mu <- shear_modulus(Vs, dens)
  # undrained bulk mod
  kap <- bulk_modulus(Vp=Vp, dens=dens, mu=mu)
  comp <- 1 / kap / 1e9
  data.frame(Mu.GPa=mu, Kappa.GPa=kap, Compress.per_Pa=comp)
}

#' @rdname Elastic.Properties
#' @export
shear_modulus <- function(Vs, dens){
  dens * Vs^2
}

#' @rdname Elastic.Properties
#' @export
bulk_modulus <- function(Vp, Vs, dens, mu=NULL){
  if (missing(Vs)){
    if (is.null(mu)) stop("did not specify 'Vs' or 'mu'")
  } else {
    mu <- shear_modulus(Vs, dens)
  }
  dens * Vp^2  -  4 * mu / 3
}

#' @rdname Elastic.Properties
#' @export
Lame_constant <- function(Vp, Vs, dens){
  dens * (Vp^2 - 2*Vs^2)
}

#' @rdname Elastic.Properties
#' @export
Poissons_ratio <- function(Vp, Vs){
  L <- Lame_constant(Vp, Vs, dens=1)
  L / (L + 2*Vp)
}

#' @rdname Elastic.Properties
#' @export
Youngs_modulus <- function(Vp, Vs, dens){
  #Vp2 <- Vp * Vp
  #Vs2 <- Vs * Vs
  mu <- shear_modulus(Vs, dens)
  L <- Lame_constant(Vp, Vs, dens)
  #E1 <- mu * (3*Vp2 - 4*Vs2) / (Vp2 - Vs2)
  E <- mu * (3*L + 2*mu) / (L + mu)
  #stopifnot(E1==E)
  return(E)
}

#' Calculate undrained pressure response to volume strain
#' @export
#' @param mu numeric; the shear modulus of the rock, in Pa
#' @param B numeric; Skempton's coefficient [0,1]
#' @param nu_u numeric; the undrained Poisson's ratio
#' @param as.areal logical; should the response be to areal strain
#' rather than volume strain?  This assumes plane stress (a 
#' traction free surface) and thus the form of vertical strain.
Undrained.Response <- function(mu, B, nu_u=NULL, as.areal=FALSE){
  #
  chk0to1(B)
  #
  if (is.null(nu_u)){
    const <- get_constants()
    nu_u <- const$Poisson$nu_u
  }
  # from a volume strain (Roel 1996, Eq 5b, p146)
  nurat <- (1 + nu_u)/(1 - 2*nu_u)
  if (as.areal) nurat <- nurat * (1 - nu_u/(1-nu_u))
  2 * mu * nurat * B / 3
}
