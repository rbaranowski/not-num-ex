#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
require(Quandl)
source("R/header.R")
source("R/misc.R")

#######################################################################################
###################### Temperature anomalies data set #################################
#######################################################################################
#**** download data
tmp <- as.data.frame(fread("http://data.giss.nasa.gov/gistemp/tabledata_v3/GLB.Ts+dSST.csv", colClasses="character"))
x <- as.numeric(t(tmp[,2:13]))
x <- x[!is.na(x)]
time <- seq(as.Date("1880/1/1"),by="month", length.out = length(x))

set.seed(seed)
#**** detect change-points with NOT SIC 
system.time(w <- not(x, contrast = "pcwsLinContMean", M=50000))
w.cpt <- features(w, penalty="sic")$cpt[[1]]

#**** plot of the data and th fitted signal
plot.data <- data.frame(time=time, x=x, fitted=predict(w), residuals=residuals(w))
file.name <- "temp_anomalies_fitted"

pl <- ggplot(plot.data, aes(x=time, y=x))+ geom_line(aes(color="data")) + 
  geom_line(aes(x=time, y=fitted, color="fitted"), linetype = 1, size=1)+ 
  scale_color_manual(values=c("gray", "red")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("time")+
  ylab("")+
  scale_x_date(limits=c(time[1],NA))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#**** plot of the residuals
file.name <- "temp_anomalies_residuals"

pl <- ggplot(plot.data, aes(x=time, y=residuals))+ geom_line(aes(color="data")) + 
  scale_color_manual(values=c("gray")) + 
  theme_bw()+
  theme(legend.position="none")+
  xlab("time")+
  ylab("")+
  scale_x_date(limits=c(time[1],NA))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()

#*** table with changepoints
dates.latex.table <- xtable(data.frame(date =  format(plot.data$time[w.cpt],"%B %Y"), comment = ""), 
                            align = c("l", "l", "l"),
                            caption = "\\label{Table:temp_anomalies_changepoints}: Change-point detected using NOT SIC methodology in the temperature anomalies data.")
print(dates.latex.table, include.rownames = FALSE, file="tables/temp_anomalies_changepoints.tex")
