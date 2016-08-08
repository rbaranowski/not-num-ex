#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
source("R/header.R")
source("R/misc.R")

#######################################################################################
###################### HOUSING PRICES #################################################
#######################################################################################
transform.data <- function(data)
  zoo(x=cbind(house.price.index=data$housePriceIndex,
              percentage.change.monthly=data$percentageChangeMonthly), 
      order.by=as.yearmon(data$Period))

#*** download data
data.tower.hamlets <- transform.data(as.data.frame(fread("http://landregistry.data.gov.uk/app/ukhpi/download/new.csv?utf8=%E2%9C%93&region=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Ftower-hamlets&location-type=local-authority&from=1995-01&to=2016-06&ai[]=housePriceIndex&ai[]=averagePrice&ai[]=percentageChange&ai[]=percentageAnnualChange&ac[]=&cs[]=salesVolume", header=TRUE)))
data.newham <- transform.data(as.data.frame(fread("http://landregistry.data.gov.uk/app/ukhpi/download/new.csv?utf8=%E2%9C%93&region=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Fnewham&location-type=local-authority&from=1995-01&to=2016-06&ai[]=housePriceIndex&ai[]=averagePrice&ai[]=percentageChange&ai[]=percentageAnnualChange&ac[]=&cs[]=salesVolume", header=TRUE)))
data.hackney <- transform.data(as.data.frame(fread("http://landregistry.data.gov.uk/app/ukhpi/download/new.csv?utf8=%E2%9C%93&region=http%3A%2F%2Flandregistry.data.gov.uk%2Fid%2Fregion%2Fhackney&location-type=local-authority&from=1995-01&to=2016-06&ai[]=housePriceIndex&ai[]=averagePrice&ai[]=percentageChange&ai[]=percentageAnnualChange&ac[]=&cs[]=salesVolume", header=TRUE)))

data.list <-list("Tower_Hamlets" = data.tower.hamlets,
                 "Newham" = data.newham,
                 "Hackney" = data.hackney)

data.list[[j]][c(52,  96, 156, 172, 230)-1,]

for(j in 1:3){
  
  x <- data.list[[j]][-1,2]
  
  set.seed(seed)
  
  #**** identify change-points with NOT SIC
  w <- not(x, method="not", contrast = "pcwsConstMeanVar")
  w.cpt <- features(w, penalty = "sic")$cpt
  
  

  #**** create data frame for plotting
  plot.data <- data.frame(time = as.Date(index(x)),
                          x = as.numeric(x),
                          fitted.mean = predict(w)[,1],
                          centered.x =  as.numeric(abs(x - predict(w, cpt=w.cpt)[,1])),
                          fitted.volatility = predict(w, cpt=w.cpt)[,2],
                          residuals =  residuals(w),
                          block = get.block(length(x), w.cpt))
  
  #**** plot the mean series
  file.name <- paste0("hpi_", names(data.list)[j], "_mean")
  
  pl <- ggplot(plot.data, aes(x=time, y=x))+ geom_line(aes(color="data")) + 
    geom_line(aes(x=time, y=fitted.mean, color="fitted", group=block), linetype = 1, size=1.5)+ 
    scale_color_manual(values=c("gray", "red")) + 
    geom_vline(xintercept = as.numeric(plot.data$time[w.cpt]), linetype = 3 )+
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
  
  #**** plot the sd series
  file.name <- paste0("hpi_", names(data.list)[j], "_vol")
  
  pl <- ggplot(plot.data, aes(x=time, y=centered.x))+ geom_line(aes(color="data")) + 
    geom_line(aes(x=time, y=fitted.volatility, color="fitted", group=block), linetype = 1, size=1.5)+ 
    scale_color_manual(values=c("gray", "red")) + 
    geom_vline(xintercept = as.numeric(plot.data$time[w.cpt]), linetype = 3 )+
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
  
}
