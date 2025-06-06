#Diatocirculartiff
Read in dia files and make circlize figures
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
library(RColorBrewer)

# in project won't look for files below top directory
#edges <- QPress::model.dia(here::here("WSS28model/Dia plots/SarahWSS1.dia"))

edges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS6.dia"))

## Examine unweighted adjacency matrix
fullmod <- adjacency.matrix(edges, labels=TRUE)

fullmodnoself <- adjacency.matrix(edges, labels=TRUE)

## remove self loops
diag(fullmodnoself) <- 0

feweredges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS1.dia"))

fewestedges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS5.dia"))

smallestmod <- adjacency.matrix(fewestedges, labels=TRUE)

smallestmodnoself <- adjacency.matrix(fewestedges, labels=TRUE)

## remove self loops
diag(smallestmodnoself) <- 0

# test plots

#circlize::chordDiagram(fullmod)

#circlize::chordDiagram(smallestmod)


# standardize group positions, widths, and colors across both models

phyto <- c("Phytoplankton")
zoo <- c("Gelatinous zoop", "Macrozoop", "Mesozoop", "Microflora", "Microzoop")
benthos <- c("Bivalves", "Megabenthos", "Meiofauna", "Other molluscs", "Shrimps",
             "Small crab other arthropoda", "Worms")
fish <- c("D piscivores", "L benthivores", "Large pelagics", "Other pelagic mesopelagic", 
          "Sharks", "Skates", "Squids")
smallpel <- c("Small pelagics")
birdsmammals <- c("Sea birds", "Toothed cetaceans", "Whales")
seals <- c("Seals")
fishery <- c("Fishery")
detritus <- c("Detritus", "Discards")

mod_C <- brewer.pal(9,"Set1")

plottiff <- function(modadjmat, plotname){
    
    mod_Colors <- data.frame(row.names(modadjmat))
    colnames(mod_Colors) <- "Focus"
    
    mod_Colors$Color[mod_Colors$Focus%in%phyto] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%zoo] <- mod_C[2]
    mod_Colors$Color[mod_Colors$Focus%in%benthos] <- mod_C[3]
    mod_Colors$Color[mod_Colors$Focus%in%fish] <- mod_C[4]
    mod_Colors$Color[mod_Colors$Focus%in%smallpel] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%birdsmammals] <- mod_C[5]
    mod_Colors$Color[mod_Colors$Focus%in%seals] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%fishery] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%detritus] <- mod_C[9]
    
    mod_Groups <- mod_Colors
    mod_Groups$Group[mod_Groups$Focus%in%phyto] <- "Phytoplankton"
    mod_Groups$Rank[mod_Groups$Focus%in%phyto] <- 1
    mod_Groups$Group[mod_Groups$Focus%in%zoo] <- "Zooplankton"
    mod_Groups$Rank[mod_Groups$Focus%in%zoo] <- 2
    mod_Groups$Group[mod_Groups$Focus%in%benthos] <- "Benthos"
    mod_Groups$Rank[mod_Groups$Focus%in%benthos] <- 3
    mod_Groups$Group[mod_Groups$Focus%in%fish] <- "Fish/Squid"
    mod_Groups$Rank[mod_Groups$Focus%in%fish] <- 4
    mod_Groups$Group[mod_Groups$Focus%in%smallpel] <- "Small pelgics"
    mod_Groups$Rank[mod_Groups$Focus%in%smallpel] <- 5
    mod_Groups$Group[mod_Groups$Focus%in%birdsmammals] <- "Birds/Mammals"
    mod_Groups$Rank[mod_Groups$Focus%in%birdsmammals] <- 6
    mod_Groups$Group[mod_Groups$Focus%in%seals] <- "Seals"
    mod_Groups$Rank[mod_Groups$Focus%in%seals] <- 7
    mod_Groups$Group[mod_Groups$Focus%in%fishery] <- "Fishery"
    mod_Groups$Rank[mod_Groups$Focus%in%fishery] <- 8
    mod_Groups$Group[mod_Groups$Focus%in%detritus] <- "Detritus"
    mod_Groups$Rank[mod_Groups$Focus%in%detritus] <- 9
    
    
    mod_edges <- cbind(modadjmat,mod_Colors)
    mod_Colors <- mod_Colors[order(mod_Colors$Color,mod_Colors$Focus),]
    mod_Colors <- matrix(mod_Colors$Color,dimnames=list(mod_Colors$Focus,"Color"))
    mod_edges <-  mod_edges[order( mod_edges$Color,mod_edges$Focus),]
    
    mod_edges$Color <- NULL
    mod_edges$Focus <- NULL
    
    
    mod_edges <- data.matrix(mod_edges)
    Border_mat <- matrix(1,nrow=nrow(mod_edges),ncol=ncol(mod_edges))
    rownames(Border_mat) <- rownames(mod_edges)
    colnames(Border_mat) <- colnames(mod_edges)
    #Border_mat[Grand_Banks_edges < 0] = 2
    Border_Col <- matrix("white",nrow=nrow(mod_edges),ncol=ncol(mod_edges))
    rownames(Border_Col) <- rownames(mod_edges)
    colnames(Border_Col) <- colnames(mod_edges)
    #Border_Col[Grand_Banks_edges < 0] = "black"
    
    Border_w <- matrix(.0001,nrow=nrow(mod_edges),ncol=ncol(mod_edges))
    rownames(Border_w) <- rownames(mod_edges)
    colnames(Border_w) <- colnames(mod_edges)
    
    tiff(here::here(paste0(plotname, ".tiff")),
         width = 8,   # Increase width to improve detail
         height = 8,  # Increase height to improve detail
         units = "in",
         res = 600)   # Increase resolution to 600 DPI for high quality
    
    # Before creating the plot, create a new blank canvas
    plot.new()  
    
    # Un-comment and print the chord diagram
    print(chordDiagram(mod_edges, directional = 0,
                       grid.col = mod_Colors,
                       row.col = mod_Colors,
                       annotationTrack = "grid", 
                       annotationTrackHeight = mm_h(5), # Adjustments in height tracking
                       preAllocateTracks = list(track.height = 0.5)
                       
    ))
    print(circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex = .6)
    }, bg.border = NA))
    
    print(legend(x = -1.1, y = 1.09, legend = c("Phytoplankton", "Zooplankton", "Benthos", "Fish/Squid", "Small pelagics", "Birds/Mammals", "Seals", "Fishery", "Detritus"),
                 lty = c(1, 1, 1, 1, 1, 1, 1, 1), lwd = c(5, 5, 5, 5, 5, 5, 5, 5),
                 col = c(mod_C[1], mod_C[2], mod_C[3], mod_C[4], mod_C[1], mod_C[6], mod_C[1], mod_C[1], mod_C[9]), ncol = 1, cex = .75, bg = NULL, box.col = NULL, bty = "n"))
    print(title(main = paste(plotname), line = -35))
    
    # Close the TIFF device
    dev.off()
    
}

plottiff(fullmod, fullmodnoself)
#plotdiff(fullmod, smallestmod, "Smallest-model")
# plottiff(fullmod, smallestmodnoself, "Smallest-model-no-diag")
plottiff(fullmod, smallestmodnoself)