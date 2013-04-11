#' Drillers' log conversions.
ssc_diagram <- function(shepard=TRUE){
  require(robCompositions)
  ssc <- data.frame(sand=c(100,0,0),
                    silt=c(0,100,0),
                    clay=c(0,0,100))
  ternaryDiag(ssc, grid=FALSE, col=NA)
}
.shepard <- function(){
  # % size: sand silt clay

  ternaryDiagAbline(data.frame(z1=c(0,0.5,0), z2=c(0.25,0.25,0.25)), col="red")
  
}