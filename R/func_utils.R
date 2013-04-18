#' Checks whether bounded between 0 and 1.
#' 
#' Many quantities exist only in unit space, that is, within the
#' range \eqn{[0,1]}.
#' 
#' This function halts operation if range of \code{X} is outside \eqn{[0,1]}.
#' 
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @param X numeric
#' @export
#' @return numeric
chk0to1 <- function(X){
  X <- as.numeric(X)
  stopifnot((X >= 0) & (X <= 1))
}