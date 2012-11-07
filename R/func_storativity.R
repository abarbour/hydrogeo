#
storativity <- function(Transmiss,Diffusiv){
	# diff equiv T/S --> S=T/D
	return(Transmiss / Diffusiv)
}
#