#' Calculate Skempton's coefficient \eqn{B}.
#' 
#' Skempton related changes in applied stress to changes in fluid pressure; this
#' is the famous constant \eqn{B}.
#' 
#' Calculates Equation (4) from Rojstaczer and Agnew (1989)
#' 
#' @section Assumptions:  
#' 
#' As noted by Rice and Cleary [1976], this definition for 
#' \eqn{B} assumes that the rock matrix is homogeneous and all the 
#' pore space is interconnected.
#'
#' @param Beta numeric; compressibility \eqn{\beta}
#' @param Phi numeric; \eqn{\Phi}
#' @param Beta_u numeric; \eqn{\beta[u]} solid matrix - undrained
#' @param Beta_f numeric; \eqn{\beta[f]} fluid
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{compressibility}}, \code{\link{hydrogeo.p}}
skempton <- function(Beta, Phi, Beta_u=NULL, Beta_f=NULL){
  const <- get_constants()
  if (is.null(Beta_u)) Beta_u <- const$compressibility$Beta_u
  if (is.null(Beta_u)) Beta_f <- const$compressibility$Beta_f
  chk0to1(Phi)
  Num. <- Beta - Beta_u
  Den. <- Num. + Phi * (Beta_f - Beta_u)
  B. <- Num./Den.
  return(B.)
}
#
