#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
source("R/header.R")
source("R/misc.R")
n <- 1000

#*** plot of the function used to construct contrast for change-point detection in the sloppe coefficient
phi1 <- phi2 <- phi3 <- rep(NA, n)

phi1[1:1000] <- phi(1:1000, 1, 1000, 125)
phi2[1:1000] <- phi(1:1000, 1, 1000, 500)
phi3[1:1000] <- phi(1:1000, 1, 1000, 750)


plot.data <- cbind(data.frame(index=1:n, phi1=phi1, phi2=phi2, phi3=phi3))

#*** plot

file.name <- "kink_basis_functions"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=phi1, color="kink"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=phi2, color="kink"), linetype = 2, size=1)+
  geom_line(aes(x=index, y=phi3, color="kink"), linetype = 3, size=1)+
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

#*** plot of the function used to construct contrast for change-point detection in the mean (Haar wavelets)
psi1 <- psi2 <- psi3 <- rep(NA, n)


psi1[1:1000] <- psi(1:1000, 1, 1000, 125)
psi2[1:1000] <- psi(1:1000, 1, 1000, 500)
psi3[1:1000] <- psi(1:1000, 1, 1000, 750)


plot.data <- cbind(data.frame(index=1:n, psi1=psi1, psi2=psi2, psi3=psi3))

#*** plot
file.name <- "jump_basis_functions"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=psi1, color="kink"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=psi2, color="kink"), linetype = 2, size=1)+
  geom_line(aes(x=index, y=psi3, color="kink"), linetype = 3, size=1)+
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



