#
dimensional_units <- function(quantity=c("diffusivity",
                                         "viscosity",
                                         "kinematic.viscosity",
                                         "permeability",
                                         "force",
                                         "stress",
                                         "strain",
                                         "compressibility")){
  quant <- match.arg(quantity)
  quant.unit <- switch(quant, 
                       diffusivity="L^2 / T",
                       viscosity="M / L / T",
                       kinematic.viscosity="L^2 / T",
                       permeability="L^2",
                       force="M * L / T^2",
                       stress="M / L / T^2",
                       strain="1",
                       compressibility="T^2 * L / M"
                       )
  message(sprintf("\t%s dimensional units: [%s]", quant, quant.unit))
  return(invisible(c(quant, quant.unit)))
}
#