#
storativity <- function(Transmiss, Diffusiv){
  # Roeloffs 1996, eq 13
	# diff equiv T/S --> S=T/D
	return(Transmiss / Diffusiv)
}
#
specific_storage <- function(Storativ, Conductiv){
  # Roeloffs 1996, p157
  return(Storativ / Conductiv)
}
#