#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
require(nmcdr)
require(Quandl)
source("R/header.R")
source("R/misc.R")

#######################################################################################
################ GBP vs USD  (changes in the vol and the mean) ########################
#######################################################################################
#**** download and transform data #
x <- Quandl("FRED/DEXUSUK", collapse="daily", start_date="2003-01-01", end_date="2016-07-15",type="zoo")
returns <- 100 * c(0,diff(log(x)))

#**** identify changepoints using n #
set.seed(seed)
system.time(e <-  nmcd(returns, 25))
e.cpt <- e$cpp

#**** identify changepoints using not #
set.seed(seed)
system.time(w <- not(returns, contrast = "pcwsConstMeanVar", M=10000, method="not", parallel=FALSE, rand.intervals = TRUE))
w.cpt <- features(w, q.max = 25, penalty="sic")$cpt

#**** create plotting data
plot.data <- data.frame(time = as.Date(index(x)),
                        x = x,
                        log.return = returns,
                        fitted.mean = predict(w)[,1],
                        centered.log.return =  abs(returns - predict(w)[,1]),
                        fitted.volatility = predict(w)[,2],
                        residuals =  residuals(w, type = "standardised"),
                        residuals.e = residuals(w, cpt=e.cpt),
                        block = get.block(length(x), w.cpt))

#**** get dates of the detected change-points
as.character(plot.data$time[w.cpt])

dates.latex.table <- xtable(data.frame(date = gsub("0(\\d\\s)", "\\1", format(plot.data$time[w.cpt],"%d %B %Y")), comment = ""), 
                            align = c("l", "l", "l"))
print(dates.latex.table, 
      include.rownames = FALSE,
      include.colnames = FALSE,
      file="tables/gbp_usd_changepoints_not.tex", 
      only.contents=TRUE, 
      hline.after=c())

dates.latex.table <- xtable(data.frame(date = gsub("0(\\d\\s)", "\\1", format(plot.data$time[e.cpt],"%d %B %Y")), comment = ""), 
                            align = c("l", "l", "l"))

print(dates.latex.table, 
      include.rownames = FALSE,
      include.colnames = FALSE,
      file="tables/gbp_usd_changepoints_nmcd.tex", 
      only.contents=TRUE, 
      hline.after=c())

#**** generate plots - oil price#
file.name <- "gbp_usd_exchange_rate"

pl <- ggplot(plot.data, aes(x=time, y=x))+ geom_line(aes(color="data")) + 
  geom_vline(xintercept = as.numeric(plot.data$time[e.cpt]), linetype = 5 )+
  geom_vline(xintercept = as.numeric(plot.data$time[w.cpt]), linetype = 3 )+
  scale_color_manual(values=c("gray", "black")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("time")+
  ylab("")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#**** generate plots - log returns + fitted mean#
file.name <- "gbp_usd_log_returns"

pl <- ggplot(plot.data, aes(x=time, y=log.return))+ geom_line(aes(color="data")) + 
  geom_line(aes(x=time, y=fitted.mean, color="fitted", group=block), linetype = 1, size=1.5)+ 
  scale_color_manual(values=c("gray", "red")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("time")+
  ylab("") 

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#**** generate plots - log returns + fitted volatility#
file.name <- "gbp_usd_centered_log_returns"

pl <- ggplot(plot.data, aes(x=time, y=centered.log.return))+ geom_line(aes(color="data")) + 
  geom_line(aes(x=time, y=fitted.volatility, color="fitted", group=block), linetype = 1, size=1.5)+ 
  scale_color_manual(values=c("gray", "red")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("time")+
  ylab("") 

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#**** generate plots - residuals#
file.name <- "gbp_usd_residuals"

pl <- ggplot(plot.data, aes(x=time, y=residuals))+ geom_line(aes(color="data")) + 
  scale_color_manual(values=c("gray", "red")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("time")+
  ylab("") 

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#**** generate plots - acf abs log returns and abs residuals#
acf.log.returns.sq <- acf(abs(plot.data$log.return)^2, plot=FALSE)
acf.residuals.sq <- acf(abs(plot.data$residuals)^2, plot=FALSE)
plot.acf.data <- data.frame(lag = acf.log.returns.sq$lag, acf.log.returns.sq=acf.log.returns.sq$acf, acf.residuals.sq=acf.residuals.sq$acf)

file.name <- "gbp_usd_log_returns_sq_acf"

pl <- ggplot(data=plot.acf.data, aes(x=lag, y=acf.log.returns.sq)) +
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept= -qnorm((1 - 0.95)/2)/sqrt(length(plot.data$residuals)), linetype="dashed") + 
  xlab("lag")+
  ylab("acf")+
  theme_bw()+
  theme(axis.title.x=element_text(vjust=0.5)) +
  theme(axis.title.y=element_text(angle=90, vjust=1))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

file.name <- "gbp_usd_residuals_sq_acf"

pl <- ggplot(data=plot.acf.data, aes(x=lag, y=acf.residuals.sq)) +
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept= -qnorm((1 - 0.95)/2)/sqrt(length(plot.data$residuals)), linetype="dashed") + 
  xlab("lag")+
  ylab("acf")+
  theme_bw()+
  theme(axis.title.x=element_text(vjust=0.5)) +
  theme(axis.title.y=element_text(angle=90, vjust=1))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()
