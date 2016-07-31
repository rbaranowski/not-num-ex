#*****  simulation models
#***** change in the mean
model.teeth <-  list(name = "teeth", 
                    cpt.type = "pcwsConstMean",
                    cpt = (1:7) * 64,
                    jump.size = 2*(-1)^{1:7},
                    start = 1,
                    n = 64 * 8)

model.blocks <-  list(name = "blocks", 
                     cpt.type = "pcwsConstMean",
                     cpt = c(205, 267, 308, 472, 512, 820, 902, 1332, 1557, 1598, 1659),
                     jump.size = diff(c(0, 14.64, -3.66, 7.32, -7.32, 10.98, -4.39, 3.29, 19.03, 7.68, 15.37, 0))/ 10,
                     start = 0,
                     n = 2048)

#****** varying slope changes
model.wave1 <-  list(name = "wave1", 
                     cpt.type = "pcwsLinContMean",
                     cpt = c(256, 512, 768, 1024, 1152, 1280, 1344),
                     jump.size = (-1)^(1:7) *(1:7)/ 64, 
                     n = 1408,
                     start=c(1,1/256))


#****** multiple changes in slope of the same magnitude
model.wave2 <-  list(name = "wave2", 
                    cpt.type = "pcwsLinContMean",
                    cpt = (1:9) * 150,
                    jump.size = (-1)^{1:16} / 32, 
                    n = 150* 10,
                    start=c(-1/2,1/64))



#****** A a mix of changes in the intercept and slope.
model.mix <-  list(name = "mix", 
                   cpt.type = "pcwsLinMean",
                   cpt = (1:7) * 256,
                   jump.size = matrix(c(c(0,-1,0,0,2,-1,0), c(1,-1,-1,1,0,1,-2)/64), ncol=2), 
                   n = 2048,
                   start=c(0,0))

#****** Changes in the mean and volatility
model.vol <-  list(name = "vol", 
                   cpt.type = "pcwsConstMeanVar",
                   cpt = (1:7) * 256,
                   jump.size = matrix(c(c(1,0,-2,0,2,-1,0), c(0,1,0,1,0,-1,1)), ncol=2), 
                   n = 2048,
                   start=c(1,1))

#****** Changes in the intercept, slope, and quadratic componsent
model.quad <-  list(name = "quad", 
                   cpt.type = "pcwsQuadMean",
                   cpt =  c(100, 250, 500),
                   jump.size = matrix(c(c(2,-2,0), c(0,-0.01, 0.01), c(0, 0, 0.00002)), ncol=3), 
                   n = 1000,
                   start=c(0, 0, 0))
