#
dimensional_units <- function(quantity=c("conductivity",
                                         "diffusivity",
                                         "transmissivity",
                                         "storativity",
                                         "specific.storage",
                                         "viscosity",
                                         "density",
                                         "kinematic.viscosity",
                                         "permeability",
                                         "force",
                                         "stress",
                                         "strain",
                                         "compressibility",
                                         "porosity",
                                         "skempton")){
  quant <- match.arg(quantity)
  quant.unit <- switch(quant, 
                       conductivity="L / T", # X
                       diffusivity="L^2 / T", # X
                       transmissivity="L^2 / T", # X
                       storativity=" ", # X
                       specific.storage="1 / L", # X
                       viscosity="M / L / T", # X
                       density="M / L^3", # X
                       kinematic.viscosity="L^2 / T", # X
                       permeability="L^2", # X
                       force="M * L / T^2", # X
                       stress="M / L / T^2", # X
                       strain="1 / 1", # X
                       compressibility="T^2 * L / M", # X
                       porosity="{0,1} / 1",
                       skempton="{0,1} / 1"
                       )
  message(sprintf("\t%s dimensional units: [%s]", quant, quant.unit))
  return(invisible(c(quant, quant.unit)))
}
#
millidarcies <- function(Perm_m2){
  return(Perm_m2 / 0.987e-12)
}
#
cmsquared <- function(msquared){
  return(msquared * 1e4)
}
#