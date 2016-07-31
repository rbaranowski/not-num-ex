#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
source("R/header.R")
source("R/misc.R")
source("R/models.R")


#*** plots for models with changes in the mean
models <-
  list(model.teeth,
       model.blocks,
       model.wave1,
       model.wave2,
       model.mix, 
       model.quad)

penalty <- "bic"

set.seed(seed)
for(model in models){
  
  x  <- sim.model(model)
  
  signal.plot.data <- data.frame(x=1:model$n,
                                 y = get.signal(model),
                                 block = get.block(model$n, model$cpt))
  
  
  w <- not(x, method = "not", contrast = model$cpt.type, rand.intervals = TRUE)
  cpt <- features(w, penalty=penalty)$cpt[[1]]
  fit <- predict(w, cpt)
  
  x.plot.data <- data.frame(x=1:model$n, y = x)
  fit.plot.data <- data.frame(x=1:model$n, y = fit, block = get.block(model$n, cpt))
  
  file.name <- paste(model$name, "example", sep="_")
  
  if(model$cpt.type == "pcwsLinContMean"){
    
    pl <- ggplot(x.plot.data, aes(x=x, y=y)) +
      geom_line(data = x.plot.data, aes(color="data")) +
      geom_line(data = signal.plot.data, aes(color="signal"), linetype = 2, size=1.5) +
      geom_line(data = fit.plot.data, aes(color="fitted" ), size=1.5) +
      scale_color_manual(values=c("gray", "red", "black")) +
      theme_bw()+
      theme(legend.position="none")+
      xlab("t")+
      ylab("")
    
    
    
  }else{
    pl <- ggplot(x.plot.data, aes(x=x, y=y)) +
      geom_line(data = x.plot.data, aes(color="data")) +
      geom_line(data = signal.plot.data, aes(color="signal", group=block), linetype = 2, size=1.5) +
      geom_line(data = fit.plot.data, aes(color="fitted", group=block ), size=1.5) +
      scale_color_manual(values=c("gray", "red", "black")) +
      theme_bw()+
      theme(legend.position="none")+
      xlab("t")+
      ylab("")
  }
  
  
  pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
  print(pl)
  dev.off()
  
  tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
  print(pl)
  dev.off()
  
}

#*** example with changes in mean and volatility
x  <- sim.model(model.vol)
w <- not(x, method = "not", contrast = "pcwsConstMeanVar", M=10000, rand.intervals = TRUE)
cpt <- features(w, penalty=penalty)$cpt[[1]]
signal <- get.signal(model.vol)

plot.data <- data.frame(time=1:model.vol$n,
                        signal.mean = signal[,1],
                        signal.vol = signal[,2],
                        fitted.mean = predict(w, cpt)[,1],
                        fitted.vol = predict(w, cpt)[,2],
                        block.true = get.block(model.vol$n, model.vol$cpt),
                        block.est = get.block(model.vol$n, cpt),
                        x = x,
                        centered.x = abs(x-signal[,1]))



pl <- ggplot(plot.data) +
  geom_line(aes(x=time, y=x, color="data")) +
  geom_line(aes(x=time, y=signal.mean, color="signal", group=block.true), linetype = 2, size=1.5)+
  geom_line(aes(x=time, y=fitted.mean, color="fitted", group=block.est ), size=1.5)+
  scale_color_manual(values=c("gray", "red", "black")) +
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")

file.name <- paste(model.vol$name, "example_mean", sep="_")


pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()


pl <- ggplot(plot.data) +
  geom_line(aes(x=time, y=centered.x, color="data")) +
  geom_line(aes(x=time, y=signal.vol, color="signal", group=block.true), linetype = 2, size=1.5)+
  geom_line(aes(x=time, y=fitted.vol, color="fitted", group=block.est ), size=1.5)+
  scale_color_manual(values=c("gray", "red", "black")) +
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")

file.name <- paste(model.vol$name, "example_volatility", sep="_")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()