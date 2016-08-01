#*****  Packages used in all code and some other parameters
require(data.table)
require(zoo)
require(tikzDevice)
require(ggplot2)
require(xtable)
require(scales)
require(igraph)
require(not)
require(dplyr)

#***** Plotting parameters
width <- 8
height <- 2

comp.times.width <- 8
comp.times.height <- 3.3

#***** Random seed used across smulations
seed <- 11111

#***** Number of MC repetitions in the simulation study and the number of available cores
mc <- 128
n.cores <- 32
