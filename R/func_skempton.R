#' Calculate Skempton's coefficient 'B'
#' 
#' Calculates Equation (4) from Rojstaczer and Agnew (1989)
#' 
#' Assumptions:  
#' The intake of the well is assumed to penetrate a porous elastic 
#' medium with uniform properties. These properties are those specified 
#' by the theory of Biot [1941] (as reexpressed by Rice and Cleary [1976] 
#' and Green and Wang [1986]), namely the compressibilities
#' of the solid phase Beta_u, the fluid phase Beta_f, 
#' and the porous matrix
#' when drained of fluid Beta, together with the Poisson's ratio nu,
#' of the matrix, the porosity phi, and the permeability kappa.
#' 
#' As noted by Rice and Cleary [1976], this definition for 
#' B assumesthat the rock matrix is homogeneous and all the 
#' pore space is interconnected.
#' 
#' @references S. Rojstaczer and D.C. Agnew (1989), 
#' “The Influence of Formation Material Properties on the Response of Water Levels in Wells to Earth Tides and Atmospheric Loading,” 
#' \emph{J. Geophys. Res.}, \strong{94} (B9), pp. 12403-12411.
#' 
skempton <- function(Beta, Beta_u, Beta_f, phi){
  Num. <- Beta - Beta_u
  Den. <- Num. + phi * (Beta_f - Beta_u)
  B. <- Num./Den.
  return(B.)
}