#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
source("R/header.R")
source("R/misc.R")

#*** plot of the function used to construct contrast for change-point detection in the sloppe coefficient
n <- 1024
phi1 <- phi2 <- phi3 <- phi4 <- rep(NA, n)

phi1[129:384] <- phi(129:384, 129, 384, 256)
phi2[257:512] <- phi(257:512, 257, 512, 480)
phi3[1:512] <- phi(1:512, 1, 512, 64)
phi4[513:1024] <- phi(513:1024, 513, 1024, 768)


plot.data <- cbind(data.frame(index=1:n, phi1=phi1, phi2=phi2, phi3=phi3, phi4=phi4))

#*** plot

file.name <- "kink_basis_functions"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=phi1, color="kink"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=phi2, color="kink"), linetype = 2, size=1)+
  geom_line(aes(x=index, y=phi3, color="kink"), linetype = 3, size=1)+
  geom_line(aes(x=index, y=phi4, color="kink"), linetype = 4, size=1)+
  scale_color_manual(values=c("black"))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=width, height=height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=width, height=height)
print(pl)
dev.off()