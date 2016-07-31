#########################################
###### source functions and models  #####
#########################################
source("R/header.R")
source("R/models.R")
source("R/misc.R")
source("R/methods.R")

set.seed(seed)
models <- list(model.teeth, model.blocks, model.wave1, model.mix, model.vol, model.quad, model.wave2)

## gaussian sigma=1
for(model in models) sim.function(model=model,
                                 mc=mc,
                                 n.cores=n.cores,
                                 sigma=1,
                                 noise="gaussian",
                                 noise.fun=rnorm,
                                 functions=sim.methods[[model$cpt.type]],
                                 functions.names=sim.methods.names[[model$cpt.type]])