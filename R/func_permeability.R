#' function does this
#' 
#' The main function to be used 
#' is \code{\link{some_func}}
#' 
#' There are also two helper functions included: 
#' \describe{
#' \item{\code{\link{some_other_func}}}{ to do something.}
#' }
#'
#'
#' @name NAMEOFFUNC
#' export
#'
#'
#' @param x  scalar, representing X with units \eqn{[m]}
#'
#' @return scalar, representing Y with units \eqn{[m]}
#' 
#'
#' @author Andrew Barbour <andy.barbour@@gmail.com> 
#' 
#' @references Hsieh, P. A., J. D. Bredehoeft, and J. M. Farr (1987),
#' Determination of aquifer transmissivity from Earth tide analysis,
#' \emph{Water Resour. Res.}, \strong{23} (10), 1824-1832, doi:10.1029/WR023i010p01824.
#' 
#' @references \url{http://www.agu.org/pubs/crossref/1987/WR023i010p01824.shtml}
#'
#' @seealso \code{\link{some_function}}, \code{\link{some_other_func}}
#'  
#' @examples
#' ### code to be run
#' this
#' # or
#' \dontrun{
#' that
#' }

permeability <- function(Transmiss, Length.scale){
  KVG <- kinvisc(grav.divide=TRUE)
  Perm. <- Transmiss * KVG / Length.scale
  return(Perm.)
}
#
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
#
well_permeab_err <- function(Transmiss, Transmiss.stderr, Ls., Ls.stderr){
  # Derived from  T * nu / Ls / g, assuming
  # only T and Ls will have uncertainties,
  # using rules from Taylor 1997.
  T. <- Transmiss
  dT. <- Transmiss.stderr
  dLs. <- Ls.stderr
  KVG <- kinvisc(grav.divide=TRUE)
  A. <- dT./Ls.
  B. <- T. * dLs. / Ls. / Ls.  # orig expression has -1, but taking norm
  dk. <- KVG * norm(cbind(A.,B.),type="F") # Euclidean norm (Froeb)
  return(dk.)
}
#
