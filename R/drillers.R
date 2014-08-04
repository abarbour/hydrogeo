#' Plot a ternary diagram of sand-silt-clay grain-size percentage.
#' 
#' The hydraulic properties of sedimentary material are roughly
#' dependent on  the proportions of grain size due to sand, silt,
#' and clay; these are common categorical distinctions found
#' in drillers logs.
#'
#' A more detailed classification system for soils is used by the
#' USDA; however, Shepard's diagram is adquate for a sand-silt-clay
#' model, and \code{shepard_plot} adds them to the current device.
#' This function is intended for a ternary diagram normalized to
#' percent particle size so that hydraulic parameters may be
#' estimated from drillers logs.
#'
#' @param shepard.diagram logical; should Shepards' (1954) classification 
#' system be overlain?
#' @param silt.lims numeric; vector of length two controlling the horizontal (silt) 
#' axis limits.
#' @param add.frame logical; should a box be drawn around it?
#' @param init logical; forces device instantiation.
#' @param ... additional parameters (unused)
#' @export
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @seealso \code{\link{xyz2ternary}}, \code{\link{hydrogeo.p}}
sand_silt_clay <- function(shepard.diagram=TRUE, silt.lims=c(-0.01,1.01), add.frame=FALSE,  ...){
  ssc <- data.frame(sand=c(100,0,0),
                    silt=c(0,100,0),
                    clay=c(0,0,100))
  gridCol <- grey(0.9)
  gridLty <- 1
  tcex <- 0.6
  textCol <- grey(0.5)
  op <- par(pty="s", oma=rep(0.1,4), mar=c(4.2,4.2,0.2,0.2), xpd=TRUE)
  on.exit(par(op))
  #
  # initialize device
  plot(0.5,0.5, col=NA, 
       #ylim=lims$y, 
       xlim=silt.lims, 
       ylim=silt.lims-0.07,
       #asp=1, 
       frame=FALSE,
       yaxs="i", ylab="", xaxs="i", xlab="", xaxt="n", yaxt="n", ...)
  if (add.frame){
    usrs <- par("usr")
    rect(xleft=usrs[1], ybottom=usrs[3], xright=usrs[2], ytop=usrs[4], 
         lwd=1, border="grey", xpd=TRUE)
  }
  # initial Ternary diagram
  polygon(xyz2ternary(ssc), col="white")
  # grid lines
  grd <- seq(10,90,by=10)
  lbls <- c(0,grd,100)
  peak <- xyz2ternary(c(0,0,100))
  zer <- rep(0,length(grd))
  XYO <- xyz2ternary(cbind(zer,grd,zer))
  XY <- xyz2ternary(cbind(zer,zer,grd))
  # right diagonal lines
  # segments go as X<------XO
  segments(x<-XY[,1], y<-XY[,2], XYO[,1], XYO[,2], lty=gridLty, col=gridCol)
  # sand percent markers
  ang <- 300
  text(c(0,x,peak[,1])-0.007, c(0,y,peak[,2])+0.012, rev(lbls), 
       col = textCol, 
       cex = tcex, srt = ang, adj=1)
  xy<-xyz2ternary(c(0,-8,53))
  text(xy[1], xy[2], "SAND SIZE, in PERCENT", cex=0.8, srt=60)
  # left diagonal lines
  segments(x<-XY[,1]+0.5, y<-rev(XY[,2]), XYO[,1], XYO[,2], lty=gridLty, col=gridCol)
  # clay percent markers
  ang <- 0
  text(c(peak[,1],x,1)+0.013, c(peak[,2],y,0), rev(lbls), 
       col = textCol, 
       cex = tcex, srt = ang, adj=0)
  xy<-xyz2ternary(c(0,53,55))
  text(xy[1], xy[2], "CLAY SIZE, in PERCENT", cex=0.8, srt=300)
  # horizontal lines
  segments(XY[,1], XY[,2], x<-rev(XY[,1]+0.5), XY[,2], lty=gridLty, col=gridCol)
  # silt percent markers
  ang <- 60
  text(rev(lbls)/100 - 0.005, c(0,zer,0)-0.01, rev(lbls), 
       col = textCol, 
       cex = tcex, srt = ang, adj=1)
  text(0.5, -0.07, "SILT SIZE, in PERCENT", cex=0.8)
  
  # Shepards soil classification
  if (shepard.diagram) shepard_plot()
  
  return(invisible(NULL))
}

#' @rdname sand_silt_clay
#' @export
shepard_plot <- function(init=FALSE){
  dc <- dev.cur()
  if (dc<=1 | init){
    plot(1,1,col=NA,xlim=c(0,1),ylim=c(0,1), frame=FALSE,
         yaxs="i", ylab="", xaxs="i", xlab="", xaxt="n", yaxt="n")
  }
  #stopifnot(dev.cur()>1) # no plotting device initialized
  ytsc <- 0.95
  tcex <- 0.8
  # % size: sand silt clay
  env <- new.env() 
  data("shepard", envir=env) 
  shepard <- env$shepard
  material <- NULL
  # strategic lines
  lines(xyz2ternary(subset(shepard, material=="from-sand"), 2))
  text(0.26, 0.3,"CLAYEY\nSAND", adj=c(1,2)/2, cex=tcex)
  text(0.36, 0.12,"SILTY\nSAND", adj=c(1,2)/2, cex=tcex)
  lines(xyz2ternary(subset(shepard, material=="from-silt"), 2))
  text(1-0.26, 0.3,"CLAYEY\nSILT", adj=c(1,2)/2, cex=tcex)
  text(1-0.36, 0.12,"SANDY\nSILT", adj=c(1,2)/2, cex=tcex)
  lines(xyz2ternary(subset(shepard, material=="from-clay"), 2))
  text(0.4, 0.55,"SANDY\nCLAY", adj=c(1,2)/2, cex=tcex)
  text(0.6, 0.55,"SILTY\nCLAY", adj=c(1,2)/2, cex=tcex)
  # pure-mixture polygons
  polygon(xyz2ternary(subset(shepard, material=="sand"), 2), col="white")
  text(0.12, 0.09,"SAND", adj=c(1,2)/2, cex=tcex)
  polygon(xyz2ternary(subset(shepard, material=="silt"), 2), col="white")
  text(1-0.12, 0.09,"SILT", adj=c(1,2)/2, cex=tcex)
  polygon(xyz2ternary(subset(shepard, material=="clay"), 2), col="white")
  text(0.5, 0.74,"CLAY", adj=c(1,2)/2, cex=tcex)
  # all-mixture
  polygon(xyz2ternary(subset(shepard, material=="sand-silt-clay"), 2), col="white")
  text(0.5, 0.33,"SAND-\nSILT-CLAY", adj=c(1,2)/2, cex=tcex)
  #
  return(invisible(NULL))
}

#' Convert triaxial data to biaxial.
#' 
#' Tri-axial data will be collapsed into a two-dimensional space, within
#' an isoceles triangle.
#' 
#' The projection goes as \deqn{
#' x'_{i} = x_{j} + x_{k}/2}
#' \deqn{
#' x'_{j} = \sqrt(3) x_{k} / 2
#' }
#' where \eqn{x} is the data in original axis system.  This assumes a normalization
#' where \eqn{\sum x = 1}
#' 
#' @seealso \code{\link{sand_silt_clay}}, \code{\link{hydrogeo.p}}
#' @param X object to be projected
#' @param col.x numeric; the column to set as the 1-axis
#' @param are.percents logical; are the values in \code{X} percentages \eqn{[0,100]} ?
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @export
xyz2ternary <- function(X, col.x=1, are.percents=TRUE){
  #if P(a,b,c), a + b + c = 1
  #then P(b + c/2, c * sqrt(3)/2)
  if (is.numeric(X)) X <- matrix(X, ncol=3)
  cx <- col.x
  cy <- cx + 1
  cz <- cy + 1
  X <- X[,cx:cz]
  if (is.vector(X)) X <- matrix(X, ncol=3)
  # normalize to iso-space
  if (are.percents) X <- X/100
  x <- X[,1]
  y <- X[,2]
  zhalf <- X[,3]/2
  rm(X)
  x <- y + zhalf
  y <- zhalf*sqrt(3)
  return(cbind(x,y))
}
