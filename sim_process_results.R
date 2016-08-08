source("R/header.R")
source("R/misc.R")
require("plyr")
#############################

get.results.df <- function(results, model){

  ddply(results,~method,function(df){
  
    tmp <- df$n.cpt - length(model$cpt)
    tmp[tmp <= -3] <- -3
    tmp[tmp >= 3] <- 3
    
    res <- rep(0,10)
    for(j in 1:7) res[j] <- round(mean(tmp==j-4) * 100)
    res[8] <- round(mean(df$MSE), 3)
    res[9] <- round(mean((abs(df$screening.dist-df$precision.dist) +  df$screening.dist + df$precision.dist)/2), 2)
    res[10] <- round(mean(df$time), 2)
      names(res) <- c("$\\leq-3$", "$-2$", "$-1$", "$0$", "$1$", "$2$", "$\\geq 3$", "MSE", "dH", "time")
    res
    
    
  })}

for(fn in list.files("results")){

  load(paste0("results/", fn))
  #######################################################################################
  ################### format table  and print to latex ##################################
  #######################################################################################
  model <- attr(results, "model")
  results <- results[complete.cases(results), ]

  
  results$n.cpts <- as.numeric(results$n.cpts)
  results$screening.dist <- as.numeric(results$screening.dist)
  results$precision.dist <- as.numeric(results$precision.dist)
  results$MSE <- as.numeric(results$MSE)
  results$time <- as.numeric(results$time)
  
  results.df <- results.table <- get.results.df(results, model)
  noise <- attr(results, "noise")
  sigma <- attr(results, "sigma")
  model.ref <- sprintf("\\ref{Model:%s}", model$name)
  
  cat("Total comp. time model ", model$name, ":", sum(results.df$time),"\n")

  
  bold <- which(results.df[["$0$"]] >= 0.9*max(results.df[["$0$"]]))
  results.table[["$0$"]][bold] <- sprintf("\\textbf{%s}",results.table[["$0$"]][bold])
  
  bold <- which(results.df[["dH"]] <= 1.1*min(results.df[["dH"]]))
  results.table[["dH"]][bold] <- sprintf("\\textbf{%s}",results.table[["dH"]][bold])

  results.table <- sapply(results.table, as.character)
  results.table
  
  results.table <- cbind(method=results.table[,1],
                         model=c(sprintf("\\multirow{%d}{*}{%s}", nrow(results.table), model.ref), rep("", nrow(results.table)-1)),
                         results.table[,-1])
  
  results.latex.table <- xtable(results.table, 
                              align = rep("l", ncol(results.table)+1))
  
  print(results.latex.table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.colnames.function=identity,
        sanitize.text.function=identity, 
        only.contents=TRUE, 
        file=sprintf("tables/%s_%s_sigma_%s_sim_results.tex", 
                     model$name, noise, sigma),
        hline.after=c())

}

