################################
#### simulation functions #####
###############################

sim.model <- function(model, sigma=1, noise.fun=rnorm, ...){
  
  if(model$cpt.type == "pcwsConstMeanVar"){
  
    signal <- get.signal(model)
    signal[,1] + signal[,2] * noise.fun(n=model$n, ...)
  
  }else get.signal(model) + sigma * noise.fun(n=model$n, ...)
  
} 



asses.fit <- function(x, model, fit, cpts, tol=0.025){
  
  cpts <- cpts[!is.na(cpts)]
  
  if(model$cpt.type == "pcwsConstMeanVar")  mse <- mean((get.signal(model)[,1]-fit[,1])^2)
  else mse <- mean((get.signal(model)-fit)^2)
  
  n <- length(x)
  n.cpts <- length(cpts)


  segments.endpoints.true <- sort(unique(c(0, model$cpt, n)))
  segments.endpoints.est <- sort(unique(c(0, cpts, n)))

  
  
  distm <- abs(matrix(rep(segments.endpoints.est, length(segments.endpoints.true)), nrow=length(segments.endpoints.est))
               -matrix(rep(segments.endpoints.true, length(segments.endpoints.est)), nrow=length(segments.endpoints.est), byrow=TRUE))
 
  screening.dist <- max(apply(distm, 2, min)) * 100 / n
  precision.dist <- max(apply(distm, 1, min)) * 100 / n

  
  return(list(MSE=mse, n.cpts=n.cpts, screening.dist=screening.dist, precision.dist=precision.dist))
  
}

get.signal <- function(model){

  if(model$cpt.type == "pcwsConstMean"){
    
    signal <- rep(0, model$n)
    
    segments <- cbind(c(1,model$cpt+1), c(model$cpt,model$n))
    signal[segments[1,1]:segments[1,2]] <- model$start[1]
  
    for(j in 2:nrow(segments)) signal[segments[j,1]:segments[j,2]] <- signal[segments[j,1]-1] + model$jump.size[j-1]
    
    
  }else if(model$cpt.type == "pcwsLinContMean"){
    
    signal <- rep(0, model$n)
    segments <- cbind(c(1,model$cpt+1), c(model$cpt,model$n))
    
    slope <- model$start[2]
    signal[segments[1,1]:segments[1,2]] <- model$start[1] + segments[1,1]:segments[1,2] * model$start[2]
    
    for(j in 2:nrow(segments)) {
      
      slope <- slope +  model$jump.size[j-1]
      
      for(k in segments[j,1]:segments[j,2]) signal[k] <- signal[k-1] + slope
    }
    
    
  } else if(model$cpt.type == "pcwsLinMean"){
  
    signal <- rep(0, model$n)
    segments <- cbind(c(1,model$cpt+1), c(model$cpt,model$n))
    
    slope <- model$start[2]
    signal[segments[1,1]:segments[1,2]] <- model$start[1] + segments[1,1]:segments[1,2] * slope

    for(j in 2:nrow(segments)) {
      
      slope <- slope +  model$jump.size[j-1,2]
      
      signal[segments[j,1]] <-  signal[segments[j-1,2]] + model$jump.size[j-1,1] 
      for(k in (segments[j,1]+1):segments[j,2]) signal[k] <- signal[k-1] + slope
      
    }
 
  } else if(model$cpt.type == "pcwsQuadMean"){
    
    signal <- rep(0, model$n)
    segments <- cbind(c(1,model$cpt+1), c(model$cpt,model$n))
    
    slope <- model$start[2]
    quad <- model$start[3]
    
    signal[segments[1,1]:segments[1,2]] <- model$start[1] + segments[1,1]:segments[1,2] * slope +  (segments[1,1]:segments[1,2])^2 * quad
    
    for(j in 2:nrow(segments)) {
      
      slope <- slope +  model$jump.size[j-1,2]
      quad <- quad +  model$jump.size[j-1,3]
      
      signal[segments[j,1]] <-  signal[segments[j-1,2]] + model$jump.size[j-1,1] 
      for(k in (segments[j,1]+1):segments[j,2]) signal[k] <- signal[k-1] + slope + (2 *  k +1) * quad
      
    }
    
  } else if(model$cpt.type == "pcwsConstMeanVar"){
    

    mu.signal <- sigma.signal <- rep(0, model$n)
    
    segments <- cbind(c(1,model$cpt+1), c(model$cpt,model$n))
    
    mu.signal[segments[1,1]:segments[1,2]] <- model$start[1]
    sigma.signal[segments[1,1]:segments[1,2]] <- model$start[2]
    
    for(j in 2:nrow(segments)){
      
      mu.signal[segments[j,1]:segments[j,2]] <- mu.signal[segments[j,1]-1] + model$jump.size[j-1,1]
      sigma.signal[segments[j,1]:segments[j,2]] <- sigma.signal[segments[j,1]-1] + model$jump.size[j-1,2]
    
    }
    
    signal <- cbind(mu.signal, sigma.signal)
  }
  
  return(signal)
}

###############################
###### main sim fun ##########
###############################

sim.function <- function(model, mc, n.cores=8, sigma, noise, noise.fun, functions, functions.names){
  
  cat(sprintf("Analysing model %s,  %s noise sigma=%s...\n", model$name, noise, sigma))
  
  
  results <- do.call(rbind, mclapply(1:mc, function(x, sigma, noise.fun){
    
    x <- sim.model(model, sigma=sigma, noise.fun=noise.fun)
    
    n.methods <- length(functions)
    
    results <- data.frame(method = functions.names,
                          MSE = numeric(n.methods),
                          n.cpts = numeric(n.methods),
                          screening.dist = numeric(n.methods),
                          precision.dist = numeric(n.methods),
                          time = numeric(n.methods)
    )
    
    colnames(results) <- c("method", "MSE", "n.cpts", "screening.dist", "precision.dist", "time")
    
    for(j in 1:length(functions)){
      
      res <- functions[[j]](x)
      mres <- asses.fit(x, model, res$fit, res$cpts)
      results$MSE[j] <- mres$MSE
      results$screening.dist[j] <- mres$screening.dist
      results$precision.dist[j] <- mres$precision.dist
      results$n.cpts[j] <- mres$n.cpts
      results$time[j] <- res$elapsed
      
    }
    
    results
    
  }, mc.cores = n.cores, sigma=sigma, noise.fun=noise.fun))
  
  
  attr(results, "mc") <- mc
  attr(results, "sigma") <- sigma
  attr(results, "noise") <- noise 
  attr(results, "model") <- model
  
  save(results, file=sprintf("results/%s_%s_sigma_%f.RData", model$name, noise, sigma))
  
}

get.block <- function(n, cpt){
  apply(matrix(rep(1:n, length(cpt)), nrow=n) > matrix(rep(cpt, n), nrow=n, byrow=TRUE), 1, sum)
}


computation.times <- function(n, contrast.type, parallel = FALSE, M=10000, ...){
  
  x <- rnorm(n)
  tic <- proc.time()[3]
  w <- not(x, contrast = contrast.type, parallel=parallel, M=M)
  toc1 <- proc.time()[3]
  features(w)
  toc2 <- proc.time()[3]
  
  return(data.frame(n=n,
                    time.solution.path=as.numeric(toc1-tic),
                    time.model.selection=as.numeric(toc2-toc1),
                    contrast.type=contrast.type,
                    parallel=parallel,
                    M=M))
  
}

pow_10_lab <- function(x) {
  paste0("$10^{",as.integer(log(x,10)),"}$")
}


phi <- function(x, s, e, b){
  
  alpha <- sqrt(6/((e - s)*(e - s + 1)*(e - s + 2)*(2-2*b^2+e+2*b*e-s+2*b*s-2*e*s)))
  beta <- sqrt(((e - b + 1) * (e - b))/((b - s)*(b - s + 1)))
  
  res <- rep(0, length(x))
  
  for(i in 1:length(x)){
    
    if((x[i] >= s) && (x[i] <=b)){
      
      res[i] = alpha * beta * (x[i] *(e + 2*b - 3*s + 2) - (b*e + b*s + 2*s - 2*s^2))
      
    }else if((b < x[i]) && (x[i] <= e)){
      
      res[i] = - (alpha / beta) * (x[i] *(3*e - 2*b - s + 2)- (2*e - b*e + 2*e^2 - b*s))
      
    }
    
  }
  
  res
}

psi <- function(x, s, e, b){
  
  l <- length(x)
  res <- rep(0, l)
  
  res[(x>=s) & (x<=b)] <- rep(sqrt((e-b)/(l * (b-s+1))), b-s+1)
  res[(x>=b+1) & (x<=e)] <- -rep(sqrt((b-s+1)/(l * (e-b))), e-b)

  res
}

contrast <- function(x){
  n <- length(x)
  
  res <- rep(NA,n)
  for(b in 2:(n-1)) res[b] <- abs(sum(x * phi(1:n, 1, n, b)))
  
  res
}


changepoints.not.rec <- function(contrasts){
  
  if(nrow(contrasts)){
    
    b <- contrasts$arg.max[1]
    s <- contrasts$start[1]
    e <- contrasts$end[1]
    
    return(rbind(changepoints.not.rec(contrasts[contrasts$end <= b,]),
                 changepoints.not.rec(contrasts[contrasts$start >= b,]),
                 c(s,e,b)))
    
    
  }else return(NULL)
  
  
}

changepoints.not <- function(contrasts, th){
  
  contrasts <- contrasts[order(contrasts$length), ]
  contrasts <- contrasts[contrasts$max.contrast > th, , drop=FALSE]
  
  changepoints.not.rec(contrasts)
}

changepoints.tree.not <- function(contrasts, th) {
  
  edges.matrix <<- matrix(0, 0, 2)
  colnames(edges.matrix) <<- c("parent", "node")
  vertices.matrix <<- matrix(0, 0, 4)
  colnames(vertices.matrix) <<- c("b", "s", "e", "th")
  
  set.seed(seed)
  contrasts <- contrasts[order(contrasts$length, sample(nrow(contrasts))), ]
  contrasts <- contrasts[contrasts$max.contrast > th, , drop=FALSE]
  
  changepoints.tree.not.rec(contrasts, NA)
  
  
  return(list(edges=edges.matrix, vertices=vertices.matrix))
  
}


changepoints.tree.not <- function(contrasts, th) {
  
  edges.matrix <<- matrix(0, 0, 2)
  colnames(edges.matrix) <<- c("parent", "node")
  vertices.matrix <<- matrix(0, 0, 4)
  colnames(vertices.matrix) <<- c("b", "s", "e", "th")
  
  set.seed(seed)
  contrasts <- contrasts[order(contrasts$length, sample(nrow(contrasts))), ]

  contrasts <- contrasts[contrasts$max.contrast > th, , drop=FALSE]
  
  changepoints.tree.not.rec(contrasts, NA)
  
  
  return(list(edges=edges.matrix, vertices=vertices.matrix))
  
}



changepoints.tree.not.rec <- function(contrasts, parent){
  
  if(nrow(contrasts)){
    
    b <- contrasts$arg.max[1]
    s <- contrasts$start[1]
    e <- contrasts$end[1]
    th <- contrasts$max.contrast[1]
    
    edges.matrix <<- rbind(edges.matrix, c(parent, b))
    vertices.matrix <<- rbind(vertices.matrix, c(b,s,e,th))
    
    
    
    changepoints.tree.not.rec(contrasts[contrasts$end <= b,], b)
    changepoints.tree.not.rec(contrasts[contrasts$start > b,], b)
    
    
  }
  
  
}



igraph.to.tikz <- function (graph, layout, scales, file) {
  
  layout <- layout / max(abs(layout))
  
  sink(file)
  
  cat("\\begin{tikzpicture}[scale=3]\n")
  
  cat("\\tikzset{\n")
  cat("\tnode/.style={circle,inner sep=1mm, draw,very thick,black,fill=white,text=black},\n")
  cat("\tsmallestNode/.style={circle,inner sep=1mm, draw,very thick,black,fill=lightgray,text=black},\n")
  cat("\tarrow/.style={very thick,black,shorten >=2pt,-stealth},\n")
  cat("}\n")
  cat("\n")
  
  
  
  ##drawing vertices
  for(j in 1:length(V(graph))) cat (sprintf ("\t\\node [%s, scale=%f] (%d) at (%f, %f)\t{%s};\n", ifelse(j==which.min(scales), "smallestNode", "node"),  scales[j], V(graph)[j], layout[j,1], layout[j,2], names(V(graph))[j]))
  
  
  ##drawing edges
  adj.matrix <- as.matrix(get.adjacency(G))
  for(i in 1:nrow(adj.matrix)) 
    for(j in 1:nrow(adj.matrix))
      if(adj.matrix[i,j]==1) cat (sprintf ("\t\\path [arrow] (%d) edge (%d);\n", i, j))
  cat("\n")
  
  cat("\\end{tikzpicture}\n")
  
  sink()
  
}

