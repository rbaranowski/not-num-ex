##### source all methods considered
require(not)
require(wbs)
require(changepoint)
require(changepoint.np)
require(ecp)
require(strucchange)
require(Segmentor3IsBack)
require(nmcdr)
require(genlasso)
require(stepR)
require(parallel)

### create a list with simulation methods for each scenario
sim.methods <- sim.methods.names <- list()

### piecewise-constant mean
sim.methods[["pcwsConstMean"]] <- list(
  not.sic=function(x){
    tic <- proc.time()
    object <- not(x, method = "not", contrast = "pcwsConstMean", parallel = FALSE)
    cpts <- features(object, penalty = "sic")$cpt
    toc <- proc.time()
    
    list(fit = predict(object, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  not.ht.sic=function(x){
    tic <- proc.time()
    object <- not(x, method = "not", contrast = "pcwsConstMeanHT", parallel = FALSE)
    cpts <- features(object, penalty = "sic")$cpt
    toc <- proc.time()
    
    list(fit = predict(object, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  wbs.sic=function(x){
    tic <- proc.time()
    
    object <- wbs(x)
    cpts <- changepoints(object)$cpt.ic$ssic.penalty
    toc <- proc.time()
    
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  pelt=function(x){
  
    tic <- proc.time()
    object <- cpt.mean(x/mad(diff(x)/sqrt(2)), method="PELT")
    cpts <- setdiff(object@cpts, length(x))
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  np.pelt=function(x){
    
    tic <- proc.time()
    object <- cpt.np(x)
    cpts <- setdiff(object@cpts, length(x))
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  ecp=function(x, K=12){
  
    tic <- proc.time()
    object <-  e.cp3o(as.matrix(x), K=K)
    cpts <- object$estimates
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  strucchange=function(x, h=0.1){
    
    tic <- proc.time()
    object <-  breakpoints(x ~ 1, h=h)
    cpts <- object$breakpoints
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  segmentor3=function(x){
  
    tic <- proc.time()
    object <-  Segmentor(x, 2)
    K <- SelectModel(object)
    cpts <- getBreaks(object)[K,1:(K-1)]
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  smuce=function(x){
    
    tic <- proc.time()
    object <-  smuceR(x, 1:length(x), family="gauss")
    cpts <- object$leftEnd[-1]
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
 },
 nmcdf=function(x, K=25){
  
    tic <- proc.time()
    object <-  nmcd(x, K)
    cpts <- object$cpp
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  }
 )

sim.methods.names[["pcwsConstMean"]] <- c("NOT",
                                      "NOT HT",
                                      "WBS",
                                      "PELT",
                                      "NP-PELT",
                                      "B\\&P",
                                      "e-cp3o",
                                      "S3IB",
                                      "SMUCE",
                                      "NMCD")

### piecewise-constant mean and variance
sim.methods[["pcwsConstMeanVar"]] <- list(
  
  not.sic = function(x){
    
    tic <- proc.time()
    object <- not(x, method = "not", contrast = "pcwsConstMeanVar", parallel = FALSE)
    cpts <- features(object, penalty = "sic")$cpt
    toc <- proc.time()
    
    list(fit = predict(object, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  pelt=function(x){
    
    tic <- proc.time()
    object <-cpt.meanvar(x, method="PELT")
    cpts <- setdiff(object@cpts, length(x))
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMeanVar"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  np.pelt=function(x){
    
    tic <- proc.time()
    object <- cpt.np(x)
    cpts <- setdiff(object@cpts, length(x))
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMeanVar"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  ecp=function(x, K=25){
    
    tic <- proc.time()
    object <-  e.cp3o(as.matrix(x), K=K)
    cpts <- object$estimates
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMeanVar"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  nmcdf=function(x, K=25){
    
    tic <- proc.time()
    object <-  nmcd(x, K)
    cpts <- object$cpp
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsConstMeanVar"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  }
  
)

sim.methods.names[["pcwsConstMeanVar"]]<- c("NOT",
                                            "PELT",
                                            "NP-PELT",
                                            "e-cp3o",
                                            "NMCD")


### piecewise-linear and continuous mean
sim.methods[["pcwsLinContMean"]]  <- list(
  not.sic=function(x){
    
    tic <- proc.time()
    object <- not(x, method = "not", contrast = "pcwsLinContMean", parallel = FALSE)
    cpts <- features(object, penalty = "sic")$cpt
    toc <- proc.time()
    
    list(fit = predict(object, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  strucchange=function(x, h=0.08){
    
    tic <- proc.time()
    tm <- 1:length(x)
    object <-  breakpoints(x ~ tm, h=h)
    cpts <- object$breakpoints
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsLinContMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  trendfiltering=function(x){
    
    tic <- proc.time()
    object <-  trendfilter(y=x, ord=1)
    tf.cv <- cv.trendfilter(object)
    toc <- proc.time()
    cpts <- which(abs(diff(object$fit[,tf.cv$i.min], differences=2)) > sqrt(.Machine$double.eps))
    
    list(fit = object$fit[,tf.cv$i.min], cpts=cpts, elapsed=(toc-tic)[3])
    
  }
)

sim.methods.names[["pcwsLinContMean"]] <- c("NOT",
                                            "B\\&P",
                                            "TF")

### change in slope and intercept
sim.methods[["pcwsLinMean"]]  <- list(
  not.sic=function(x){
    
    tic <- proc.time()
    object <- not(x, method = "not", contrast = "pcwsLinMean", parallel = FALSE)
    cpts <- features(object, penalty="sic")$cpt
    toc <- proc.time()
    
    list(fit = predict(object, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  strucchange=function(x, h=0.08){
    
    tic <- proc.time()
    tm <- 1:length(x)
    object <-  breakpoints(x ~ tm, h=h)
    cpts <- object$breakpoints
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsLinMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  trendfiltering=function(x){
    
    tic <- proc.time()
    object <-  trendfilter(y=x, ord=1)
    tf.cv <- cv.trendfilter(object)
    toc <- proc.time()
    cpts <- which(abs(diff(object$fit[,tf.cv$i.min], differences=2)) > sqrt(.Machine$double.eps))
    
    list(fit = object$fit[,tf.cv$i.min], cpts=cpts, elapsed=(toc-tic)[3])
    
  }
)

sim.methods.names[["pcwsLinMean"]] <-  c("NOT",
                                         "B\\&P",
                                         "TF")

### change in slope and intercept and quadratic coeff
sim.methods[["pcwsQuadMean"]]  <- list(
  not.sic=function(x){
    
    tic <- proc.time()
    object <- not(x, method = "not", contrast = "pcwsQuadMean", parallel = FALSE)
    cpts <- features(object, penalty = "sic")$cpt
    toc <- proc.time()
    
    list(fit = predict(object, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  strucchange=function(x, h=0.08){
    
    tic <- proc.time()
    tm <- 1:length(x)
    tm2 <- (1:length(x))^2
    object <-  breakpoints(x ~ tm+tm2, h=h)
    cpts <- object$breakpoints
    toc <- proc.time()
    
    #creating not like object to be able to use the predict function
    w <- list(x=x)
    class(w) <- "not"
    w$contrast <- "pcwsQuadMean"
    
    list(fit = predict(w, cpt=cpts), cpts=cpts, elapsed=(toc-tic)[3])
  },
  trendfiltering=function(x){
    
    tic <- proc.time()
    object <-  trendfilter(y=x, ord=2)
    tf.cv <- cv.trendfilter(object)
    toc <- proc.time()
    cpts <- which(abs(diff(object$fit[,tf.cv$i.min], differences=3)) > sqrt(.Machine$double.eps))
    
    list(fit = object$fit[,tf.cv$i.min], cpts=cpts, elapsed=(toc-tic)[3])
    
  }
)

sim.methods.names[["pcwsQuadMean"]] <-  c("NOT",
                                          "B\\&P",
                                          "TF")
