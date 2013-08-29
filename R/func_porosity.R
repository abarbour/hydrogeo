#' Calculate porosity.
#' 
#' Calculates Equation (20) from Rojstaczer and Agnew (1989),
#' an inversion of Eqn. (4) -- Skempton's coeff
#'
#' @param Beta numeric; compressibility \eqn{\beta}
#' @param B. numeric; Skempton's coefficient \eqn{B}
#' @param Beta_u numeric; \eqn{\beta[u]} solid matrix - undrained
#' @param Beta_f numeric; \eqn{\beta[f]} fluid
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{skempton}}, \code{\link{hydrogeo}}
porosity <- function(Beta, B., Beta_u=NULL, Beta_f=NULL){
  const <- hydrogeo:::.constants
  if (is.null(Beta_u)) Beta_u <- const$compressibility$Beta_u
  if (is.null(Beta_u)) Beta_f <- const$compressibility$Beta_f
  chk0to1(B.)
  Num. <- (Beta - Beta_u) * (1 - B.)
  Den. <- (Beta_f - Beta_u) * B.
  Phi <- Num./Den.
  return(Phi)
}
#
