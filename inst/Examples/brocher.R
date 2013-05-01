Vps <- matrix(seq(0,9.5,by=.1))
reps <- 1

# Vs

lbls <- c(as.character(unlist(formals(as.Vs))$emp.fit)[-1],"averaged")
res.vs <- matrix(apply(X=Vps, MARGIN=2, FUN=function(x){as.Vs(rep(x,reps), do.all=TRUE)$Vs}),ncol=length(lbls))
image(y=Vps, x=xlbl<-seq_along(lbls), z=t(res.vs), , xaxt="n", xlab="", ylab="Vp, km/s",
      col = gray((0:8)/10),
      main="Vs scaling relationships")
axis(1,at=xlbl,labels=lbls)
abline(v=xlbl+0.5, lty=1)

# Density

lbls <- c(as.character(unlist(formals(as.RockDensity))$emp.fit)[-1],"averaged")
res.dens <- matrix(apply(X=Vps, MARGIN=2, FUN=function(x){as.RockDensity(rep(x,reps), do.all=TRUE)$dens}), ncol=length(lbls))

image(y=Vps, x=xlbl<-seq_along(lbls), z=t(res.dens), , xaxt="n", xlab="", ylab="Vp, km/s",
      col = gray((0:8)/10),
      main="Density scaling relationships")
axis(1,at=xlbl,labels=lbls)
abline(v=xlbl+0.5)

matplot(x=Vps,res.vs[,1:3],type="l", lwd=c(1,3,3),col=c("red","black","blue"), ylab="Vs")
VpVs <- 1/sweep(res.vs[,1:3],MARGIN=1,Vps,'/')
matplot(x=Vps,VpVs, type="l", log="y", lwd=c(1,3,3),col=c("red","black","blue"), ylab="Vs/Vp")

matplot(x=Vps,res.dens[,1:3],type="l", lwd=c(1,3,3),col=c("red","black","blue"), ylab="Density")