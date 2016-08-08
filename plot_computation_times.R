#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
source("R/header.R")
source("R/misc.R")

n.set <- (10^(2:7))
M.set <- (10^(2:7))
################################################################################
############ Plot  computation times for the not package #######################
################################################################################


load(file ="times/computation_times_fixed_M.Rdata")

levels(results$contrast.type) <- c("pcwsConstMean", "pcwsLinContMean", "pcwsLinMean", "pcwsConstMeanVar")
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsConstMean"] <- "\\ref{Scen:change_in_mean}"
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsLinContMean"] <- "\\ref{Scen:change_in_slope}"
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsLinMean"] <- "\\ref{Scen:change_in_mean_and_slope}"
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsConstMeanVar"] <- "\\ref{Scen:change_in_mean_and_variance}"

file.name <- "computation_time_sol_path_fixed_M"


pl <- ggplot(results, aes(x=n, y=time.solution.path, shape=contrast.type)) +
  geom_point(size=3)+
  geom_line()+
  scale_x_log10(breaks = n.set, labels=pow_10_lab)+
  scale_y_log10(breaks = c(0.1, 1, 10, 100))+
  ylab("")+
  xlab("$T$")+
  theme_bw()+
  theme(legend.position="bottom")+
  guides(shape=guide_legend(title=""))+
  theme(legend.key = element_rect(color = 'NA'))+
  theme(legend.key.width=unit(3,"line"))+
  theme(legend.text.align = 1)+
  theme(legend.key.width = unit(3,"line"))



pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=comp.times.width, height=comp.times.height)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=comp.times.width, height=comp.times.height)
print(pl)
dev.off()

file.name <- "computation_time_sol_path_fixed_T"

load(file ="times/computation_times_fixed_T.Rdata")

levels(results$contrast.type) <- c("pcwsConstMean", "pcwsLinContMean", "pcwsLinMean", "pcwsConstMeanVar")
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsConstMean"] <- "\\ref{Scen:change_in_mean}"
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsLinContMean"] <- "\\ref{Scen:change_in_slope}"
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsLinMean"] <- "\\ref{Scen:change_in_mean_and_slope}"
levels(results$contrast.type)[levels(results$contrast.type) == "pcwsConstMeanVar"] <- "\\ref{Scen:change_in_mean_and_variance}"

pl <- ggplot(results, aes(x=M, y=time.solution.path, shape=contrast.type)) +
  geom_point(size=3)+
  geom_line()+
  scale_x_log10(breaks = n.set, labels=pow_10_lab)+
  scale_y_log10(breaks = c(0.1, 1, 10, 100))+
  ylab("")+
  xlab("$M$")+
  theme_bw()+
  theme(legend.position="bottom")+
  guides(shape=guide_legend(title=""))+
  theme(legend.key = element_rect(color = 'NA'))+
  theme(legend.key.width=unit(3,"line"))+
  theme(legend.text.align = 1)+
  theme(legend.key.width = unit(3,"line"))




pdf(file = paste("pdf/",file.name,".pdf", sep=""), width=comp.times.width, height=comp.times.height)
print(pl)
dev.off()


tikz(file = paste("tikz/",file.name,".tex", sep=""),  width=comp.times.width, height=comp.times.height)
print(pl)
dev.off()

