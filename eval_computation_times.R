#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
require(Quandl)
source("R/header.R")
source("R/misc.R")

################################################################################
################ evalute computation times for the not package #################
################################################################################

comp.mc <- 10

n.set <- (10^(2:7))
M.set <- (10^(2:7))

contrast.type.set <- c("pcwsConstMean",
                   "pcwsLinMean",
                   "pcwsLinContMean",
                   "pcwsConstMeanVar")

#***** fixed M, n growing

results <- list()

k <- 1
for(contrast.type in contrast.type.set)
  for(n in n.set)
    for(j in 1:mc){
      results[[k]] <- computation.times(n, contrast.type, parallel = TRUE)
      k <- k+1
    }

results <- do.call(rbind, results)
results <- as.data.frame(group_by(results, n, contrast.type, M ) %>% summarise(time.solution.path = mean(time.solution.path), time.model.selection = mean(time.model.selection)))

save(results, file ="times/computation_times_fixed_M.Rdata")

#***** fixed n, M growing

n <- 10000
results <- list()

k <- 1
for(contrast.type in contrast.type.set)
  for(M in M.set)
    for(j in 1:mc){
      results[[k]] <- computation.times(n, contrast.type, parallel = TRUE, M=M)
      k <- k+1
    }

results <- do.call(rbind, results)
results <- as.data.frame(group_by(results, n, contrast.type, M ) %>% summarise(time.solution.path = mean(time.solution.path), time.model.selection = mean(time.model.selection)))
save(results, file ="times/computation_times_fixed_T.Rdata")
