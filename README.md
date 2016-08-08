# Narrowest-Over-Threshold Approach For Change-Point Detection: Simulation code"
The R code for simulations and real data analyses discussed in "Narrowest-Over-Threshold Change-Point De-
tection" by R. Baranowski, Y. Chen, and P. Fryzlewicz (2016) available from http://personal.lse.ac.uk/baranows/not/not.pdf.

# Dependencies
The code for plots and tables presented in the paper requires the following R packages (see "R/header.R"), all of which are available from CRAN.
- 'not' implementing the Narrowest-Over-Threshold methodology
- 'data.table', 'zoo', 'dplyr' for data manipulation
- 'ggplot2', 'scales', 'igraph' for plots
- 'tikzDevice' for exporting plots to latex
- 'xtable' for exporting tables to latex

To run the real data analyses and the simulation study, the following packages are additionally required (see "R/methods.R").
- 'wbs' for the WBS method
- 'changepoint' for the PELT method
- 'changepoint.np' for the NP-PELT method
- 'ecp' for the e-cp3o method
- 'strucchange' for the B&P method
- 'nmcdr' for the NMCD method
- 'genlasso' for the TF method
- 'stepR' for the SMUCE method
- 'parallel' for running the simulations in parallel

For an explanation of the abbreviations above, see the paper.