#' Convert between material properties and seismic velocities.
#' 
#' Uses the empirical relations from Brocher (2005)
#' to convert material properties into seismic velocities, and
#' the other way around.
#' A function to
#' convert density into porosity is also provided.
#' 
#' @details
#' 
#' The empirical relations are based on regression curves
#' based on material type, and
#' thus restricted to within specific ranges of the independent variable,
#' for specific material;
#' the valid ranges are:
#' 
#' \tabular{llllr}{
#'  \emph{Valid rock/lithology types} \tab \emph{Function} \tab \emph{Validity} \tab \tab \emph{Eqn}\cr
#'  \tab \tab \emph{Lower} \tab \emph{Upper} \tab \cr

#'  \strong{\eqn{V[p]} to \eqn{\rho} conversions} \tab \tab \tab \tab \cr
#'  (\strong{km/s} to \strong{g/cm^3}) \tab \tab \tab \tab \cr
#'    All except mafic crustal and CA-rich\tab \code{\link{dens_nafe_drake}} \tab \eqn{1.5} \tab \eqn{8.0}\tab 1\cr
#'    Sedimentary \tab \code{\link{dens_gardner}} \tab \eqn{1.5} \tab \eqn{6.1}\tab 2\cr
#'    Cyrstalline (except volcanic and monomineralic) \tab \code{\link{dens_christensen_mooney}} \tab \eqn{5.5} \tab \eqn{7.5}\tab 3\cr
#'    Basalt, diabase, and gabbro \tab \code{\link{dens_godfrey}} \tab \eqn{5.9} \tab \eqn{7.1}\tab 4\cr

#'  \tab \tab \tab \tab \cr
#'  \strong{\eqn{\rho} to \eqn{V[p]} conversions} \tab \tab \tab \tab \cr
#'  (\strong{g/cm^3} to \strong{km/s}) \tab \tab \tab \tab \cr
#'   (see \code{\link{dens_nafe_drake}}) \tab \code{\link{vp_brocher_ludwig}} \tab \eqn{2.0} \tab \eqn{3.5} \tab 5\cr

#'  \tab \tab \tab \tab \cr
#'  \strong{\eqn{V[p]} to \eqn{V[s]} conversions} \tab \tab \tab \tab \cr
#'  (\strong{km/s} to \strong{km/s}) \tab \tab \tab \tab \cr
#'    All except CA-rich and mafic, gabbros, \tab \code{\link{vs_brocher}} \tab \eqn{1.5} \tab \eqn{8.0} \tab 6\cr
#'       and serpentinites                   \tab \tab \tab \tab \cr
#'    Clay-rich sedimentary \tab \code{\link{vs_castagna}} \tab \eqn{1.5} \tab \eqn{4.25} \tab 7\cr
#'    CA-rich (inc. dolomites and anorthosites), \tab \code{\link{vs_brocher_mafic}} \tab \eqn{5.25} \tab \eqn{7.25} \tab 8\cr
#'       mafic, and gabbros                      \tab \tab \tab \tab \cr

#'  \tab \tab \tab \tab \cr
#'  \strong{\eqn{V[s]} to \eqn{V[p]} conversions} \tab \tab \tab \tab \cr
#'  (\strong{km/s} to \strong{km/s}) \tab \tab \tab \tab \cr
#'    (See \code{\link{vs_brocher}}) \tab \code{\link{vp_brocher}} \tab \eqn{0.0} \tab \eqn{4.5} \tab 9\cr

#'  \tab \tab \tab \tab \cr
#'  \strong{\eqn{V[p]} to \eqn{\nu} conversions} \tab \tab \tab \tab \cr
#'  (\strong{km/s} to \strong{[dimensionless]}) \tab \tab \tab \tab \cr
#'    (See \code{\link{vs_brocher}}) \tab \code{\link{nu_brocher}} \tab \eqn{1.5} \tab \eqn{8.5} \tab 11\cr
#'    (See \code{\link{vs_brocher}}) \tab \code{\link{nu_brocher_ludwig}} \tab \eqn{1.5} \tab \eqn{8.5} \tab 12\cr
#' }
#' \emph{The field \code{Eqn} gives the equation number from Brocher (2005).}
#'
#' The functions \code{\link{nu_continuum}} and \code{\link{vpvs_continuum}} return
#' Poisson's ratio and the \eqn{V[p]/V[s]} ratio for a continuum 
#' representation; is missing arguments they default to a Poisson solid (\eqn{\nu=1/4}
#' or \eqn{V[p]/V[s] = \sqrt{3}}).
#'
#' The parameter \code{null_val} can be set to fill points where any conversion
#' lies outside
#' the given range.  
#'
#' @name brocher
#' @rdname brocher
#' @param X numeric; An object to convert
#' @param nu numeric; Poisson's ratio
#' @param Dens.bulk numeric; the density of the 
#' @param Dens.solid numeric; the density of the 
#' @param Dens.fluid numeric; the density of the 
#' @param non.neg logical; should
#' @param dens.units character; the units of the \code{Dens.} args.
#' @param return.percent logical; should
#' @param return.voidfrac logical; should
#' @param verbose logical; should messages be printed?
#' @param Vp numeric; P-wave velocity in km/s
#' @param Vs numeric; S-wave velocity in km/s 
#' @param Vp.units character; the units of \code{Vp}
#' @param pos numeric; vector of indices or distances for \code{Vp}
#' @param emp.fit character; the empirical fit to use [overridden if \code{(do.all | all.average)}]
#' @param do.all logical; should all available epirical scalings be calculated?
#' @param all.average logical; should, once all available epirical scalings are calculated, the collection of results be averaged?
#' @param null_val the value to fill in place of conversions outside of
#' the valid range (see \strong{Details})
#' @param ... additional parameters
#' @return A \code{data.frame} for the \code{as.} functions; numeric otherwise.
#' @author Andrew J. Barbour <andy.barbour@@gmail.com> 
#' @references Brocher, T. M. (2005). 
#' Empirical relations between elastic wavespeeds and density in the Earth's crust. 
#' \emph{Bulletin of the Seismological Society of America}, \strong{95} (6), 2081-2092.
#' @seealso \code{\link{kms}}, \code{\link{hydrogeo}}
NULL


#' @rdname brocher
#' @export
#' @examples
#' From.Vp(rnorm(5)+4)
From.Vp <- function(Vp, ...){
  Dens <- subset(as.RockDensity(Vp, ...), meth=="Averaged")
  meth <- NULL
  Vs <- subset(as.Vs(Vp, ...), meth=="Averaged")
  # add Poissons ratio
  Full <- merge(Vs, Dens, by=c("pos","Vp","meth"), all=TRUE)
  #Full <- cbind(Vs,Dens)
  return(Full)
}

#' @rdname brocher
#' @export
as.RockDensity <- function(Vp, pos=seq_along(Vp), 
                           emp.fit=c("nafe_drake", "gardner", "christensen_mooney", "godfrey"), 
                           do.all=TRUE, all.average=do.all,
                           Vp.units=NULL, verbose=TRUE){
  if (verbose) message("Converting Vp to Density (Brocher) ...")
  all.meth <- paste0("dens_", emp.fit)
  # init dataframe
  toret <- data.frame(pos=numeric(0), Vp=numeric(0), dens=numeric(0), meth=character(0))
  # convert if asked-to
  Vp <- switch(match.arg(Vp.units, c("kmsec","ftmusec","ftsec")),
               kmsec=Vp, 
               ftmusec=kms(Vp, is.mus=TRUE), 
               ftsec=kms(Vp) )
  
  doConv <- function(X., FUN., ...){sapply(X=X., FUN=FUN., ...)}
  
  if (do.all | all.average){
    for (meth in all.meth){
      if (verbose) message(meth)
      CONVFUN <- function(...) UseMethod(meth)
      toret <- rbind(toret, data.frame(pos=pos, Vp=Vp, dens=as.numeric(doConv(Vp, CONVFUN)), meth=meth))
    }
    if (all.average){
      if (verbose) message("averaging all relations")
      #require(reshape2)
      moretoret <- reshape2::dcast(toret, pos+Vp ~ . , value.var=c("dens"), mean, trim=0.05, na.rm=TRUE)
      names(moretoret) <- names(toret)[1:3]
      moretoret$meth <- "Averaged"
      toret <- rbind(toret, moretoret)
    }
  } else {
    if (length(all.meth)>1){
      meth <- all.meth[1]
      warning(sprintf("forced %s because 'emp.fit' was not specified", meth))
      if (verbose) on.exit(message("consider specifying an emprical model to use; or set 'do.all=TRUE'"))
    }
    meth <- match.arg(meth, all.meth) # redundant
    if (verbose) message(meth)
    CONVFUN <- function(...) UseMethod(meth)
    toret <- rbind(toret, data.frame(pos=pos, Vp=Vp, meth=meth, dens=as.numeric(doConv(Vp, CONVFUN))))
  }
  return(toret)
}

#
# Vp to density
#

#' @rdname brocher
#' @export
dens_nafe_drake <- function(Vp, null_val=NA) { UseMethod("dens_nafe_drake") }
#' @rdname brocher
#' @aliases dens_nafe_drake.default
#' @method dens_nafe_drake default
#' @S3method dens_nafe_drake default
dens_nafe_drake.default <- function(Vp, null_val=NA){
  Vp2 <- Vp*Vp
  Vp3 <- Vp2*Vp
  Vp4 <- Vp3*Vp
  Vp5 <- Vp4*Vp
  # Brocher 2005 eq 1
  Dens <- null_val
  if ((Vp>=1.5) & (Vp<=8.5)) Dens <- 1.6612*Vp - 0.4721*Vp2 + 0.0671*Vp3 - 0.0043*Vp4 + 0.000106*Vp5
  return(Dens)
}

#' @rdname brocher
#' @export
dens_gardner <- function(Vp, null_val=NA) { UseMethod("dens_gardner") }
#' @rdname brocher
#' @aliases dens_gardner.default
#' @method dens_gardner default
#' @S3method dens_gardner default
dens_gardner.default <- function(Vp, null_val=NA){
  # Brocher 2005 eq 2
  Dens <- null_val
  if ((Vp>1.5) & (Vp<6.1)) Dens <- 1.74*Vp**0.25
  return(Dens)
}


#' @rdname brocher
#' @export
dens_christensen_mooney <- function(Vp, null_val=NA) { UseMethod("dens_christensen_mooney") }
#' @rdname brocher
#' @aliases dens_christensen_mooney.default
#' @method dens_christensen_mooney default
#' @S3method dens_christensen_mooney default
dens_christensen_mooney.default <- function(Vp, null_val=NA){
  # Brocher 2005 eq 3
  Dens <- null_val
  if ((Vp>=5.5) & (Vp<=7.5)) Dens <- 0.541 + 0.3601*Vp
  return(Dens)
}

#' @rdname brocher
#' @export
dens_godfrey <- function(Vp, null_val=NA) { UseMethod("dens_godfrey") }
#' @rdname brocher
#' @aliases dens_godfrey.default
#' @method dens_godfrey default
#' @S3method dens_godfrey default
dens_godfrey.default <- function(Vp, null_val=NA){
  # Brocher 2005 eq 4
  Dens <- null_val
  if ((Vp>=5.9) & (Vp<=7.1)) Dens <- 2.4372 + 0.0761*Vp
  return(Dens)
}

#
# Vp to Vs
#
#' @rdname brocher
#' @export
#' @examples
#' as.Vs(rnorm(5)+4)
as.Vs <- function(Vp, pos=seq_along(Vp), 
                  emp.fit=c("brocher", "castagna", "brocher_mafic"), 
                  do.all=TRUE, all.average=do.all,
                  Vp.units=NULL, verbose=TRUE){
  if (verbose) message("Converting Vp to Vs (Brocher) ...")
  all.meth <- paste0("vs_", emp.fit)
  # init dataframe
  toret <- data.frame(pos=numeric(0), Vp=numeric(0), Vs=numeric(0), meth=character(0))
  # convert if asked-to
  Vp <- switch(match.arg(Vp.units, c("kmsec","ftmusec","ftsec")),          
               kmsec=Vp, ftmusec=kms(Vp, is.mus=TRUE), ftsec=kms(Vp) )

  doConv <- function(X., FUN., ...){sapply(X=X., FUN=FUN., ...)}
  
  if (do.all | all.average){
    for (meth in all.meth){
      if (verbose) message(meth)
      CONVFUN <- function(...) UseMethod(meth)
      toret <- rbind(toret, data.frame(pos=pos, Vp=Vp, Vs=as.numeric(doConv(Vp, CONVFUN)), meth=meth))
    }
    if (all.average){
      if (verbose) message("averaging all relations")
      #require(reshape2)
      moretoret <- reshape2::dcast(toret, pos+Vp ~ . , value.var=c("Vs"), mean, trim=0.05, na.rm=TRUE)
      names(moretoret) <- names(toret)[1:3]
      moretoret$meth <- "Averaged"
      toret <- rbind(toret, moretoret)
    }
  } else {
    if (length(all.meth)>1){
      meth <- all.meth[1]
      warning(sprintf("forced %s because 'emp.fit' was not specified", meth))
      if (verbose) on.exit(message("consider specifying an emprical model to use; or set 'do.all=TRUE'"))
    }
    meth <- match.arg(meth, all.meth) # redundant
    if (verbose) message(meth)
    CONVFUN <- function(...) UseMethod(meth)
    toret <- rbind(toret, data.frame(pos=pos, Vp=Vp, meth=meth, Vs=as.numeric(doConv(Vp, CONVFUN))))
  }
  return(toret)
}

#' @rdname brocher
#' @export
vs_brocher <- function(Vp, null_val=NA) { UseMethod("vs_brocher") }
#' @rdname brocher
#' @aliases vs_brocher.default
#' @method vs_brocher default
#' @S3method vs_brocher default
vs_brocher.default <- function(Vp, null_val=NA){
  Vp2 <- Vp*Vp
  Vp3 <- Vp2*Vp
  Vp4 <- Vp3*Vp
  # Brocher 2005 eq 6
  Vs <- null_val
  if ((Vp>=1.5) & (Vp<=8.0)) Vs <- 0.7858 - 1.2344*Vp + 0.7949*Vp2 - 0.1238*Vp3 + 0.0064*Vp4
  return(Vs)
}

#' @rdname brocher
#' @export
vs_castagna <- function(Vp, null_val=NA) { UseMethod("vs_castagna") }
#' @rdname brocher
#' @aliases vs_castagna default
#' @method vs_castagna default
#' @S3method vs_castagna default
vs_castagna.default <- function(Vp, null_val=NA){
  # Brocher 2005 eq 7 (simp)
  Vs <- null_val
  if ((Vp>=1.5) & (Vp<=4.25)) Vs <- 0.862069*Vp - 1.172414
  return(Vs)
}

#' @rdname brocher
#' @export
vs_brocher_mafic <- function(Vp, null_val=NA) { UseMethod("vs_brocher_mafic") }
#' @rdname brocher
#' @aliases vs_brocher_mafic.default
#' @method vs_brocher_mafic default
#' @S3method vs_brocher_mafic default
vs_brocher_mafic.default <- function(Vp, null_val=NA){
  # Brocher 2005 eq 8 (simp)
  Vs <- null_val
  if ((Vp>=5.25) & (Vp<=7.25)) Vs <- 2.88 + (0.52*Vp - 2.73)
  return(Vs)
}

#
# Vs to Vp
#
#' @rdname brocher
#' @export
as.Vp <- function(X, pos=seq_along(X),
                  emp.fit=c("brocher", "brocher_ludwig"), 
                  do.all=FALSE, all.average=FALSE,
                  Vp.units=NULL, verbose=TRUE){
  # brocher: Vs
  # brocher_ludwig: Dens.bulk
  .NotYetImplemented()
}


#' @rdname brocher
#' @export
vp_brocher <- function(Vs, null_val=NA) { UseMethod("vp_brocher") }
#' @rdname brocher
#' @aliases vp_brocher.default
#' @method vp_brocher default
#' @S3method vp_brocher default
vp_brocher.default <- function(Vs, null_val=NA){
  Vs2 <- Vs*Vs
  Vs3 <- Vs2*Vs
  Vs4 <- Vs3*Vs
  # Brocher 2005 eq 9
  Vp <- null_val
  if ((Vs>=0) & (Vs<=4.5)) Vp <- 0.9409 + 2.0947*Vs - 0.8206*Vs2 + 0.2683*Vs3 - 0.0251*Vs4
  return(Vp)
}

#' @rdname brocher
#' @export
vp_brocher_ludwig <- function(Dens.bulk, null_val=NA) { UseMethod("vp_brocher_ludwig") }
#' @rdname brocher
#' @aliases vp_brocher_ludwig.default
#' @method vp_brocher_ludwig default
#' @S3method vp_brocher_ludwig default
vp_brocher_ludwig.default <- function(Dens.bulk, null_val=NA){
  rho2 <- Dens.bulk*Dens.bulk
  rho3 <- rho2*Dens.bulk
  rho4 <- rho3*Dens.bulk
  rho5 <- rho5*Dens.bulk
  # Brocher 2005 eq 6
  Vp <- null_val
  if ((Dens.bulk>=2) & (Dens.bulk<=3.5)) Vp <- 39.128*Dens.bulk - 63.064*rho2 + 37.083*rho3 - 9.1819*rho4 + 0.8228*rho5
  return(Vp)
}


#
# VContinuum Vp/Vs & Poissons ratio
#

#' @rdname brocher
#' @export
vpvs_continuum <- function(nu) { UseMethod("vpvs_continuum") }
#' @rdname brocher
#' @aliases vpvs_continuum.default
#' @method vpvs_continuum default
#' @S3method vpvs_continuum default
vpvs_continuum.default <- function(nu){
  # Brocher 2005 eq 10, inverted for Vp/Vs ratio
  if (missing(nu)) nu <- hydrogeo:::.constants$Poisson$nu
  Num <- 2*(nu - 1)
  Den <- 2*nu - 1
  VpVs <- sqrt(Num/Den)
  return(VpVs)
}

#' @rdname brocher
#' @export
nu_continuum <- function(Vp, Vs) { UseMethod("nu_continuum") }
#' @rdname brocher
#' @aliases nu_continuum.default
#' @method nu_continuum default
#' @S3method nu_continuum default
nu_continuum.default <- function(Vp, Vs){
  # Brocher 2005 eq 10
  if (missing(Vp) | missing(Vs)){
	Rat <- hydrogeo:::.constants$Poisson$VpVs
  } else {
	Rat <- Vp / Vs
  }
  Rat2 <- Rat*Rat
  Num <- Rat2 - 2
  Den <- Rat2 - 1
  nu <- Num / Den / 2
  return(nu)
}

#
# Vp to Poissons ratio
#
#' @rdname brocher
#' @export
as.PoissonsRatio <- function(Vp, pos=seq_along(Vp), 
                             emp.fit=c("brocher", "brocher_ludwig"), 
                             do.all=FALSE, all.average=FALSE,
                             Vp.units=NULL, verbose=TRUE){
  .NotYetImplemented()
}

#' @rdname brocher
#' @export
nu_brocher <- function(Vp, null_val=NA) { UseMethod("nu_brocher") }
#' @rdname brocher
#' @aliases nu_brocher.default
#' @method nu_brocher default
#' @S3method nu_brocher default
nu_brocher.default <- function(Vp, null_val=NA){
  nu <- null_val
  Vp2 <- Vp*Vp
  Vp3 <- Vp2*Vp
  # Brocher 2005 eq 11
  if ((Vp>=1.5) & (Vp<=8.5)) nu <- 0.8835 - 0.315*Vp + 0.0491*Vp2 - 0.0024*Vp3
  return(nu)
}
#' @rdname brocher
#' @export
nu_brocher_ludwig <- function(Vp, null_val=NA) { UseMethod("nu_brocher_ludwig") }
#' @rdname brocher
#' @aliases nu_brocher_ludwig.default
#' @method nu_brocher_ludwig default
#' @S3method nu_brocher_ludwig default
nu_brocher_ludwig.default <- function(Vp, null_val=NA){
  nu <- null_val
  Vp2 <- Vp*Vp
  Vp3 <- Vp2*Vp
  # Brocher 2005 eq 12
  if ((Vp>=1.5) & (Vp<=8.5)) nu <- 0.769 - 0.226*Vp + 0.0316*Vp2 - 0.0014*Vp3
  return(nu)
}

#' @rdname brocher
#' @export
as.Porosity <- function(Dens.bulk, Dens.solid=2.8, Dens.fluid=1, 
                        non.neg=TRUE,
                        dens.units=NULL,
                        return.percent=FALSE, return.voidfrac=FALSE, verbose=TRUE){
  if (verbose) message("Converting Density to Porosity...")
  if (verbose) message(sprintf("Solid-matrix density used:  %f  g/cc", Dens.solid))
  sc <- switch(match.arg(dens.units, c("gcc")), gcc=1)
  if (abs(sc-1)>0){
    if (verbose) message("scaling Densities...")
    Dens.bulk <- Dens.bulk * sc
    Dens.fluid <- Dens.fluid * sc
    Dens.solid <- Dens.solid * sc
  }
  ##
  DelDensNum <- Dens.solid - Dens.bulk
  if (non.neg){
    DelDensNum <- abs(DelDensNum)
  } else {
    if (DelDensNum<0){
      warning(sprintf("Negative relative density! (ref dens  %f  g/cc)", Dens.solid))
    }
  }
  ##
  DelDensDen <- Dens.solid - Dens.fluid
  stopifnot(abs(DelDensDen)>0)
  ##
  Porosity <- DelDensNum / DelDensDen
  Voidfrac <- 1 - Porosity
  ##
  if (return.percent){
    Porosity <- 100 * Porosity
    Voidfrac <- 100 * Voidfrac
  }
  return(data.frame(porosity=Porosity, void.frac=Voidfrac))
}
