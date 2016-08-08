#######################################################################################
################ load required packages and source code  ##############################
#######################################################################################
source("R/header.R")
source("R/misc.R")
###### plots for the motivating example in the paper

n <- 1000
sigma <- 0.05

signal <- rep(0, n)

signal[1:350] <- 1:350 / 350
signal[350  + 1:300] <- 1
signal[651:1000] <- 350:1 / 350

intervals <- matrix(c(1,1000, 10, 245, 225, 450, 500, 750, 740, 950, 450, 550), ncol=2, byrow=TRUE)
M <- nrow(intervals)

set.seed(seed)
x <- signal + sigma*rnorm(n)
o.not <- not(x, contrast="pcwsLinContMean", method="not", parallel=FALSE, rand.intervals = FALSE, intervals=intervals)
o.wbs <- not(x, contrast="pcwsLinContMean", method="max", parallel=FALSE, rand.intervals = FALSE, intervals=intervals)

contrasts <- apply(o.not$contrasts[,1:2], 1, function(int){
  
  res <- rep(NA, n)
  res[int[1]:int[2]] <- contrast(x[int[1]:int[2]])
  res
  
})

colnames(contrasts) <- paste0("int", 1:M)


plot.data <- cbind(data.frame(index=1:n,
                              x=x, 
                              signal=signal, 
                              not.fitted = predict(o.not, cpt=features(o.not, method = "th", th = 0.5)$cpt),
                              wbs.fitted = predict(o.not, cpt=features(o.wbs, method = "th", th = 1)$cpt),
                              as.data.frame(contrasts)))

#***** plot with contrasts
file.name <- "illustrative_example_contrasts"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=int1, color="standard"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=int2, color="not"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=int3, color="not"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=int4, color="not"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=int5, color="not"), linetype = 1, size=1)+
  geom_line(aes(x=index, y=int6, color="not"), linetype = 1, size=1)+
  scale_color_manual(values=c("gray", "black"))+
  scale_y_sqrt()+
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

#**** plot fitted signals
file.name <- "illustrative_example_fitted"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=x, color="data"), linetype = 1)+
  geom_line(aes(x=index, y=signal, color="signal"), linetype = 2, size=2)+
  geom_line(aes(x=index, y=not.fitted, color="not"), linetype = 1, size=2)+
  geom_line(aes(x=index, y=wbs.fitted, color="standard"), linetype = 4, size=2)+
  scale_color_manual(values=c("gray",  "red",  "black", "navy"))+
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

#**** plots for models with one changepoint

w <- not(signal, contrast = "pcwsLinContMean", method="not", M=M, parallel=FALSE, rand.intervals = FALSE, intervals=intervals)

plot.data <- cbind(data.frame(index=1:n,
                              signal=signal,
                              fitted1 = predict(w,  cpt=350),
                              fitted2 = predict(w,  cpt=500),
                              fitted3 = predict(w,  cpt=651)))

rss <- c(sum((signal-predict(w,  cpt=350))^2),
         sum((signal-predict(w,  cpt=500))^2),
         sum((signal-predict(w,  cpt=651))^2))

file.name <- "motivating_example_one_cpt_1"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=signal, color="signal"), linetype = 2)+
  geom_line(aes(x=index, y=fitted1, color="fitted"), linetype = 1, size=1)+
  scale_color_manual(values=c("red", "black"))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")+
  annotate("text", x = Inf,
           y = Inf,
           vjust=2,
           hjust=1.1,
           label = sprintf("$\\mbox{Error}=%.1f$", rss[1]))

pdf(file = paste("pdf/",file.name,".pdf", sep=""), width= 0.6*width, height= 0.4*width)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""), width= 0.6*width, height= 0.4*width)
print(pl)
dev.off()


file.name <- "motivating_example_one_cpt_2"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=signal, color="signal"), linetype = 2)+
  geom_line(aes(x=index, y=fitted2, color="fitted"), linetype = 1, size=1)+
  scale_color_manual(values=c("red", "black"))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")+
  annotate("text", x = Inf,
           y = Inf,
           vjust=2,
           hjust=1.1,
           label = sprintf("$\\mbox{Error}=%.1f$", rss[2]))


pdf(file = paste("pdf/",file.name,".pdf", sep=""), width= 0.6*width, height= 0.4*width)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""), width= 0.6*width, height= 0.4*width)
print(pl)
dev.off()

file.name <- "motivating_example_one_cpt_3"

pl <- ggplot(plot.data) +
  geom_line(aes(x=index, y=signal, color="signal"), linetype = 2)+
  geom_line(aes(x=index, y=fitted3, color="fitted"), linetype = 1, size=1)+
  scale_color_manual(values=c("red", "black"))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("t")+
  ylab("")+
  annotate("text", x = Inf,
           y = Inf,
           vjust=2,
           hjust=1.1,
           label = sprintf("$\\mbox{Error}=%.1f$", rss[3]))


pdf(file = paste("pdf/",file.name,".pdf", sep=""), width= 0.6*width, height= 0.4*width)
print(pl)
dev.off()

tikz(file = paste("tikz/",file.name,".tex", sep=""), width= 0.6*width, height= 0.4*width)
print(pl)
dev.off()

#**** create table with contrast functions
contrasts.table <- o.not$contrasts
colnames(contrasts.table) <- c("$s$", 
                               "$e$",
                               "$e-s+1$", 
                               "$\\argmax_{s\\leq b \\leq e}\\cont{s}{e}{b}{\\Yb}$",
                               "$\\max_{s\\leq b \\leq e}\\cont{s}{e}{b}{\\Yb}$")

contrasts.table[,5] <- round(contrasts.table[,5],2)

contrasts.table.latex <- xtable(contrasts.table[ ,], 
                                align = c("c", "c", "c", "c", "c", "c"))

print(contrasts.table.latex, 
      include.rownames = FALSE,
      include.colnames = FALSE,
      hline.after=c(),
      only.contents = TRUE,
      file="tables/illustrative_example_contrasts.tex", 
      sanitize.colnames.function=identity)

#**** plot segmentation tree -- plot last three segments
n.path <- length(o.not$solution.path$th)

for(i in 1:4){
  
  segmentation.tree <- changepoints.tree.not(o.not$contrasts, o.not$solution.path$th[n.path-i+1])
  G <- graph.tree(n=0,children=2)
  
  G <- G + vertices(segmentation.tree$vertices[,1])
  if(nrow(segmentation.tree$edges)>1) for(j in 2:nrow(segmentation.tree$edges)) G <- G + edge(as.character(segmentation.tree$edges[j,1]), as.character(segmentation.tree$edges[j,2]))
  
  
  node.size <-5*setNames(segmentation.tree$vertices[,4], as.character(segmentation.tree$vertices[,1]))
  lay <- layout.reingold.tilford(G, params=list(root=as.character(segmentation.tree$edges[1,2])))
  V(G)$color <- "grey"
  E(G)$color <- "black"
  
  rownames(lay) <- V(G)
  
  scales <- 0.5 + segmentation.tree$vertices[,4] / max(segmentation.tree$vertices[,4])
  
  igraph.to.tikz(G, lay, scales, paste0("tikz/illustrative_example_seg_tree",i, ".tex"))

  
}



