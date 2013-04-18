#' Permeability.
#' 
#' A collection of functions used to estimate the permeability
#' of a porous medium, from a well-sensing system, and associated
#' uncertainties.
#'
#' @param Transmiss numeric; the transmissivity, with units \eqn{[X]}
#' @param Length.scale numeric; the length scale for permeability calculation, in \eqn{[m]}
#' @param Len_screen numeric; the length of the screened portion of the well
#' @param stdErrors numeric; standard errors for \code{Transmiss} and \code{Len_screen}
#' @param Transmiss.stderr numeric; the standard error associated with \code{Transmiss}
#' @param Len_screen.stderr numeric; the standard error associated with \code{Len_screen}
#' @return numeric
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{transmissivity}}, \code{\link{hydrogeo}}
permeability <- function(Transmiss, Length.scale){
  KVG <- kinvisc(grav.divide=TRUE)
  Perm. <- Transmiss * KVG / Length.scale
  return(Perm.)
}

#' @rdname permeability
#' @export
well_permeab <- function(Transmiss, Len_screen, stdErrors=NULL){
  Perm. <- permeability(Transmiss, Len_screen)
	toret <- list(Permeab=Perm., Permeab.stderr=NA)
	if (!(NA %in% stdErrors) & !(is.null(stdErrors))){
	  E. <- as.vector(stdErrors)
	  stopifnot(length(E.)==2)
	  toret$Permeab.stderr=well_permeab_err(Transmiss, E.[1], Len_screen, E.[2])
	}
	return(toret)
}

#' @rdname permeability
#' @export
well_permeab_err <- function(Transmiss, Transmiss.stderr, Len_screen, Len_screen.stderr){
  # Derived from  T * nu / Ls / g, assuming
  # only T and Ls will have uncertainties,
  # using rules from Taylor 1997.
  Tr. <- Transmiss
  dTr. <- Transmiss.stderr
  Ls. <- Len_screen
  dLs. <- Len_screen.stderr
  KVG <- kinvisc(grav.divide=TRUE)
  A. <- dTr./Ls.
  B. <- Tr. * dLs. / Ls. / Ls.  # orig expression has -1, but taking norm
  dk. <- KVG * norm(cbind(A.,B.),type="F") # Euclidean norm (Froeb)
  return(dk.)
}
#
