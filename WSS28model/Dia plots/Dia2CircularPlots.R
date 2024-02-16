# Read in dia files and make circlize figures
# based on code in Geret DePiper's repo for summer flounder modeling
# https://github.com/NEFSC/READ-SSB-DePiper_Summer_Flounder_Conceptual_Models/blob/master/circulargraphrcode_Summer_Flounder_v2.R
#
# Citation: 
# Geret DePiper, Sarah Gaichas, Brandon Muffley, Greg Ardini, Jeffrey Brust, 
# Jessica Coakley, Kiley Dancy, G Warren Elliott, Dustin C Leaning, Douglas Lipton, 
# Jason McNamee, Charles Perretti, Kirby Rootes-Murdy, Michael J Wilberg, 
# Learning by doing: collaborative conceptual modelling as a path forward in 
# ecosystem-based management, ICES Journal of Marine Science, Volume 78, Issue 4, 
# August 2021, Pages 1217–1228, https://doi.org/10.1093/icesjms/fsab054
#
#

library(QPress)
library(chorddiag)
library(circlize)

# in project won't look for files below top directory
#edges <- QPress::model.dia(here::here("WSS28model/Dia plots/SarahWSS1.dia"))

edges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS1.dia"))

## Examine unweighted adjacency matrix
fullmod <- adjacency.matrix(edges, labels=TRUE)

feweredges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS6.dia"))

smallestmod <- adjacency.matrix(feweredges, labels=TRUE)

# test plots

circlize::chordDiagram(fullmod)

circlize::chordDiagram(smallestmod)


# standardize group positions, widths, and colors across both models


