#########################################
###### source functions and models  #####
#########################################
source("R/header.R")
source("R/models.R")
source("R/misc.R")
source("R/methods.R")

set.seed(seed)
# models <- list(model.teeth, model.blocks, model.wave1, model.mix, model.vol, model.quad, model.wave2)
models <- list(model.vol)
## student sigma=1
for(model in models) sim.function(model=model,
                                  mc=mc,
                                  n.cores=n.cores,
                                  sigma=1,
                                  noise="student",
             			                noise.fun=function(n) rt(n, df=5) * sqrt(3/5),
                                  functions=sim.methods[[model$cpt.type]],
                                  functions.names=sim.methods.names[[model$cpt.type]])