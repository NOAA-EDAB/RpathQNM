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
library(RColorBrewer)

# in project won't look for files below top directory
#edges <- QPress::model.dia(here::here("WSS28model/Dia plots/SarahWSS1.dia"))

edges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS6.dia"))

## Examine unweighted adjacency matrix
fullmod <- adjacency.matrix(edges, labels=TRUE)

feweredges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS1.dia"))

fewestedges <- QPress::model.dia(here::here("WSS28model/Dia plots/WSS5.dia"))

smallestmod <- adjacency.matrix(fewestedges, labels=TRUE)

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

# Function for any of these models
# This is Geret's original pdf maker

pdfmod <- function(modadjmat, plotname){
    
    mod_Colors <- data.frame(row.names(modadjmat))
    colnames(mod_Colors) <- "Focus"
    
    mod_Colors$Color[mod_Colors$Focus%in%phyto] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%zoo] <- mod_C[2]
    mod_Colors$Color[mod_Colors$Focus%in%benthos] <- mod_C[3]
    mod_Colors$Color[mod_Colors$Focus%in%fish] <- mod_C[4]
    mod_Colors$Color[mod_Colors$Focus%in%smallpel] <- mod_C[5]
    mod_Colors$Color[mod_Colors$Focus%in%birdsmammals] <- mod_C[6]
    mod_Colors$Color[mod_Colors$Focus%in%seals] <- mod_C[7]
    mod_Colors$Color[mod_Colors$Focus%in%fishery] <- mod_C[8]
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
    
    pdf(here::here(paste0(plotname,".pdf")),width = 6.69291, height=6.69291)
    print(chordDiagram(mod_edges, directional=0,
                       grid.col = mod_Colors,
                       row.col = mod_Colors,
                       link.lty = Border_mat,
                       link.lwd = Border_w,
                       link.border = Border_Col,
                       annotationTrack="grid",preAllocateTracks= list(track.height=0.5)))
    
    print(circos.trackPlotRegion(track.index=1, panel.fun= function (x,y){
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim),ylim[1],sector.name,facing="clockwise", niceFacing=TRUE, adj =c(0,0.5), cex=.6)
    }, bg.border=NA) )
    
    print(legend(x=-1.1,y = 1.09,legend = c("Phytoplankton","Zooplankton","Benthos","Fish/Squid","Small pelgics","Birds/Mammals","Seals","Fishery", "Detritus"),
                 lty= c(1,1,1,1,1,1,1,1), lwd=c(5,5,5,5,5,5,5,5),
                 col =c(mod_C[1],mod_C[2],mod_C[3],mod_C[4],mod_C[5],mod_C[6],mod_C[7],mod_C[8], mod_C[9]), ncol=1, cex = .75, bg = NULL, box.col=NULL, bty = "n"))
    print(title(main=paste(plotname), line=-35))
    dev.off()
    
}

pdfmod(fullmod, "Fullmodel")
pdfmod(smallestmod, "Smallmodel")

# this function plots individual models on their own scale, not great for comparisons

plotmod <- function(modadjmat, plotname){
    
    mod_Colors <- data.frame(row.names(modadjmat))
    colnames(mod_Colors) <- "Focus"
    
    mod_Colors$Color[mod_Colors$Focus%in%phyto] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%zoo] <- mod_C[2]
    mod_Colors$Color[mod_Colors$Focus%in%benthos] <- mod_C[3]
    mod_Colors$Color[mod_Colors$Focus%in%fish] <- mod_C[4]
    mod_Colors$Color[mod_Colors$Focus%in%smallpel] <- mod_C[5]
    mod_Colors$Color[mod_Colors$Focus%in%birdsmammals] <- mod_C[6]
    mod_Colors$Color[mod_Colors$Focus%in%seals] <- mod_C[7]
    mod_Colors$Color[mod_Colors$Focus%in%fishery] <- mod_C[8]
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
    
    png(here::here(paste0(plotname,".png")))
    print(chordDiagram(mod_edges, directional=0,
                       grid.col = mod_Colors,
                       row.col = mod_Colors,
                       link.lty = Border_mat,
                       link.lwd = Border_w,
                       link.border = Border_Col,
                       annotationTrack="grid",preAllocateTracks= list(track.height=0.5)))
    
    print(circos.trackPlotRegion(track.index=1, panel.fun= function (x,y){
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim),ylim[1],sector.name,facing="clockwise", niceFacing=TRUE, adj =c(0,0.5), cex=.6)
    }, bg.border=NA) )
    
    print(legend(x=-1.1,y = 1.09,legend = c("Phytoplankton","Zooplankton","Benthos","Fish/Squid","Small pelgics","Birds/Mammals","Seals","Fishery", "Detritus"),
                 lty= c(1,1,1,1,1,1,1,1), lwd=c(5,5,5,5,5,5,5,5),
                 col =c(mod_C[1],mod_C[2],mod_C[3],mod_C[4],mod_C[5],mod_C[6],mod_C[7],mod_C[8], mod_C[9]), ncol=1, cex = .75, bg = NULL, box.col=NULL, bty = "n"))
    print(title(main=paste(plotname), line=-35))
    dev.off()
    
}

plotmod(fullmod, "Full model")
plotmod(smallestmod, "Smallest model")


# This function plots the full model but only colors links in the compared model.

plotdiff <- function(full, comp, compname){
    
    mod_Colors <- data.frame(row.names(full))
    colnames(mod_Colors) <- "Focus"
    
    mod_Colors$Color[mod_Colors$Focus%in%phyto] <- mod_C[1]
    mod_Colors$Color[mod_Colors$Focus%in%zoo] <- mod_C[2]
    mod_Colors$Color[mod_Colors$Focus%in%benthos] <- mod_C[3]
    mod_Colors$Color[mod_Colors$Focus%in%fish] <- mod_C[4]
    mod_Colors$Color[mod_Colors$Focus%in%smallpel] <- mod_C[5]
    mod_Colors$Color[mod_Colors$Focus%in%birdsmammals] <- mod_C[6]
    mod_Colors$Color[mod_Colors$Focus%in%seals] <- mod_C[7]
    mod_Colors$Color[mod_Colors$Focus%in%fishery] <- mod_C[8]
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
    
    
    mod_edges <- cbind(full,mod_Colors)
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
    
    # define colors for mod2
    mod2_Colors <- data.frame(row.names(comp))
    colnames(mod2_Colors) <- "Focus"
    
    mod2_Colors$Color[mod2_Colors$Focus%in%phyto] <- mod_C[1]
    mod2_Colors$Color[mod2_Colors$Focus%in%zoo] <- mod_C[2]
    mod2_Colors$Color[mod2_Colors$Focus%in%benthos] <- mod_C[3]
    mod2_Colors$Color[mod2_Colors$Focus%in%fish] <- mod_C[4]
    mod2_Colors$Color[mod2_Colors$Focus%in%smallpel] <- mod_C[5]
    mod2_Colors$Color[mod2_Colors$Focus%in%birdsmammals] <- mod_C[6]
    mod2_Colors$Color[mod2_Colors$Focus%in%seals] <- mod_C[7]
    mod2_Colors$Color[mod2_Colors$Focus%in%fishery] <- mod_C[8]
    mod2_Colors$Color[mod2_Colors$Focus%in%detritus] <- mod_C[9]
    
    compdf  <-  data.frame(from = rep(rownames(comp), times = ncol(comp)),
                    to = rep(colnames(comp), each = nrow(comp)),
                    value = as.vector(comp),
                    stringsAsFactors = FALSE) |>
        dplyr::filter(value != 0) |>
        dplyr::left_join(mod2_Colors, by=c("from" = "Focus")) |>
        dplyr::select(-value)
    
    png(here::here(paste0(compname,".png")))
    print(chordDiagram(mod_edges, directional=0,
                       grid.col = mod_Colors,
                       col = compdf,
                       row.col = mod_Colors,
                       link.lty = Border_mat,
                       link.lwd = Border_w,
                       link.border = Border_Col,
                       annotationTrack="grid",preAllocateTracks= list(track.height=0.5)))
    
    print(circos.trackPlotRegion(track.index=1, panel.fun= function (x,y){
        xlim = get.cell.meta.data("xlim")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
        circos.text(mean(xlim),ylim[1],sector.name,facing="clockwise", niceFacing=TRUE, adj =c(0,0.5), cex=.6)
    }, bg.border=NA) )
    
    print(legend(x=-1.1,y = 1.09,legend = c("Phytoplankton","Zooplankton","Benthos","Fish/Squid","Small pelgics","Birds/Mammals","Seals","Fishery", "Detritus"),
                 lty= c(1,1,1,1,1,1,1,1), lwd=c(5,5,5,5,5,5,5,5),
                 col =c(mod_C[1],mod_C[2],mod_C[3],mod_C[4],mod_C[5],mod_C[6],mod_C[7],mod_C[8], mod_C[9]), ncol=1, cex = .75, bg = NULL, box.col=NULL, bty = "n"))
    print(title(main=paste(compname), line=-35))
    dev.off()
    
}

plotdiff(fullmod, smallestmod, "Smallest model")
