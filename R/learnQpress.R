library(QPress)
parse.text(c("A <-* B","C *-> A","C <- D","D -> C","B *--* C","A <--- D"))
#From To Group Type Pair
#1     A  B     0    N    1
#2     C  A     0    P    2
#4     D  C     0    P    4
#5     B  C     1    N    5
#7     B  A     0    P    1
#8     A  C     0    N    2
#9     D  C     0    P    3
#11    C  B     1    N    5
#12    D  A     2    P    6

qpr.ex<-parse.text(c("A <-* B","C *-> A","C <- D","D -> C","B *--* C","A <--- D"))
modelA <- parse.text(c("V-*V", "P-*P", "V*->H", "H*->P"))
modelA
#From To Group Type Pair
#1    V  V     0    N    1
#2    P  P     0    N    2
#3    V  H     0    P    3
#4    H  P     0    P    4
#7    H  V     0    N    3
#8    P  H     0    N    4

adjacency.matrix(modelA, labels = T)
#H  P  V
#H  0 -1  1
#P  1 -1  0
#V -1  0 -1

simA <- system.simulate(1000, modelA)
impact.barplot(simA)

#attempt to read in GOA pollock spreadsheet using MM2Qpress
setwd("~/Documents/0_Data/NCEAS_GOAfutures")
#setwd("~/Data/Projects/NCEAS_GOAfutures")
pollmm_v1 <- read.csv("GOApoll_mm_v1.csv")

data<-pollmm_v1
poll1<-MM2Qpress(data)
poll1<-dput(poll1)
poll1Model<-parse.text(poll1)

#doesnt work without this
poll1el<-enforce.limitation(poll1Model)

poll1sim<-system.simulate(1000, poll1el)
impact.barplot(poll1sim)

# pollock version 2
pollmm_v2 <- read.csv("GOApoll_mm_v2.csv")

data<-pollmm_v2
poll2<-MM2Qpress(data)
poll2<-dput(poll2)
poll2Model<-parse.text(poll2)

#doesnt work without this
poll2el<-enforce.limitation(poll2Model)

poll2sim<-system.simulate(1000, poll2el)
impact.barplot(poll2sim)

# Arrowtooth model
atfmm_v1 <- read.csv("GOAatf_qpresslables.csv")

data2<-atfmm_v1
atf1<-MM2Qpress(data2)
atf1<-dput(atf1)
atf1Model<-parse.text(atf1)

#doesnt work without this
atf1el<-enforce.limitation(atf1Model)

atf1sim<-system.simulate(1000, atf1el)
impact.barplot(atf1sim)

# arrowtooth model with corrections
atfmm_v2 <- read.csv("GOAatf_v2.csv")

data2<-atfmm_v2
atf2<-MM2Qpress(data2)
atf2<-dput(atf2)
atf2Model<-parse.text(atf2)

#doesnt work without this
atf2el<-enforce.limitation(atf2Model)

atf2sim<-system.simulate(1000, atf2el)
impact.barplot(atf2sim)

# arrowtooth model with corrections--add pollock environment
atfmm_v3 <- read.csv("GOAatf_v3.csv")

data3<-atfmm_v3
atf3<-MM2Qpress(data3)
atf3<-dput(atf3)
atf3Model<-parse.text(atf3)

#doesnt work without this
atf3el<-enforce.limitation(atf3Model)

atf3sim<-system.simulate(1000, atf3el)
impact.barplot(atf3sim)


# Halibut model
halmm_v1 <- read.csv("GOAhalibut_qpresslablesv1.csv")

data3<-halmm_v1
hal1<-MM2Qpress(data3)
hal1<-dput(hal1)
hal1Model<-parse.text(hal1)

#doesnt work without this
hal1el<-enforce.limitation(hal1Model)

hal1sim<-system.simulate(1000, hal1el)
impact.barplot(hal1sim)

# Merged model
GOAallmm_v1 <- read.csv("GOAmerge_qpress.csv")

data4<-GOAallmm_v1
GOAall1<-MM2Qpress(data4)
GOAall1<-dput(GOAall1)
GOAall1Model<-parse.text(GOAall1)

#doesnt work without this
GOAall1el<-enforce.limitation(GOAall1Model)

GOAall1sim<-system.simulate(1000, GOAall1el)
impact.barplot(GOAall1sim)

# Merged model revised to remove duplicate environment->pollock links
GOAallmm_v2 <- read.csv("GOAmerge_qpress_v2.csv")

data5<-GOAallmm_v2
GOAall2<-MM2Qpress(data5)
GOAall2<-dput(GOAall2)
GOAall2Model<-parse.text(GOAall2)

#doesnt work without this
GOAall2el<-enforce.limitation(GOAall2Model)

GOAall2sim<-system.simulate(1000, GOAall2el)
impact.barplot(GOAall2sim)


## RUNS BELOW DONE AT NCEAS, SEE REDO for atf3el and GOAall2el
## final mods use poll2el, atf2el, hal1el, GOAall1el
## rename GOApoll, GOAatf, GOAhal, GOAall
## scenarios "PDO"=+1, "Phytoplankton"=+1, "PSC"=+1, "PSC"=-1
## outnames for files

#models with uncertain links
atfU<-data.frame(From=c("Water_Temp", "Arrowtooth_Larvae", "Arrowtooth_active_vessels", "Arrowtooth_total_landings"),
                 To=c("Phytoplankton", "Arrowtooth_Juv", "Arrowtooth_total_landings", "Arrowtooth_price"), 
                 Group=c(0,0,0,0), 
                 Type=c("U", "U", "U", "U"), 
                 Pair=c(40, 41, 42, 43))

atf2ModU <- rbind(atf2Model, atfU)
atf2Uel<-enforce.limitation(atf2ModU)

polU<-data.frame(From=c("Transport",  "Pollock_active_vessels", "Pollock_Total_landings"),
                 To=c("Juv_pollock",  "Pollock_Total_landings", "Pollock_price"), 
                 Group=c(0,0,0), 
                 Type=c("U", "U", "U"), 
                 Pair=c(41, 42, 43))

poll2ModU <- rbind(poll2Model, polU)
poll2ModU$To <- factor(poll2ModU$To, levels=levels(poll2ModU$From))
poll2Uel<-enforce.limitation(poll2ModU)

halU<-data.frame(From=c("Transport", "Halibut_Larvae", "Halibut_active_vessels", "Groundfish_trawl_active_vessels", "Total_N_charter_businesses"),
                 To=c("Pollock", "Halibut_Juv", "Halibut_longline_catch_IFQ_CDQ", "Groundfish_trawl_catch_PSC", "Charter_business_profit"), 
                 Group=c(0,0,0,0,0), 
                 Type=c("U", "U", "U", "U", "U"), 
                 Pair=c(70, 71, 72, 73, 74))

hal1ModU <- rbind(hal1Model, halU)
hal1ModU$To <- factor(hal1ModU$To, levels=levels(hal1ModU$From))
hal1Uel<-enforce.limitation(hal1ModU)

allU<-data.frame(From=c("Water_Temp","Transport", "Pollock_active_vessels", "Pollock_Total_landings","Transport", "Arrowtooth_Larvae", "Arrowtooth_active_vessels", "Arrowtooth_total_landings", "Halibut_Larvae", "Halibut_active_vessels", "Groundfish_trawl_active_vessels", "Total_N_charter_businesses"),
                 To=c("Phytoplankton","Pollock_Juv", "Pollock_Total_landings", "Pollock_price", "Pollock", "Arrowtooth_Juv", "Arrowtooth_total_landings", "Arrowtooth_price", "Halibut_Juv", "Halibut_longline_catch_IFQ_CDQ", "Groundfish_trawl_catch_PSC", "Charter_business_profit"), 
                 Group=c(0,0,0,0,0,0,0,0,0,0,0,0), 
                 Type=c("U", "U","U", "U", "U", "U", "U","U", "U", "U", "U", "U"), 
                 Pair=c(121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132))

GOAall1ModU <- rbind(GOAall1Model, allU)
GOAall1ModU$To <- factor(GOAall1ModU$To, levels=levels(GOAall1ModU$From))
GOAall1Uel<-enforce.limitation(GOAall1ModU)


set.seed(88)

qprPerturbSave <- function(mod, scenario, outname){
  
  ### Jamie's deconstructed qpress code
  # pollock
  A <- adjacency.matrix(mod)
  #A 
  
  ## Function to generate the community matrix
  s <- community.sampler(mod)
  
  ## Function to check the validation condition
  #press <- press.validate(GB2el,
  #                        perturb=c("Forage_Fish"=-1),
  #                        monitor=c("Commercial_Pelagic_Fishery"=1))
  
  #Press/Perturbation with no monitoring)
  press <- press.validate(mod,
                          perturb=c(scenario), monitor=F)
  
  ## Function to define the perturbation scenario
  impact <- press.impact(mod,perturb=c(scenario))
  ## Use 10000 simulations
  n.sims <- 1000
  results <- 0
  i <- 0
  while(i < n.sims) {
    
    ## Randomly choose edges to retain
    z <- s$select(runif(1))
    ## Sample community matrix
    W <- s$community()
    
    ## Check press condition and stability
    if(!(press(W) && stable.community(W))) next
    
    ## Monitor impact post press
    imp <- impact(W)
    imp[abs(imp)<1e-07]=0     ######J REUM ADDED THIS LINE, get rid of very small responses, following RAYMOND 2011, line 181 from is example1.R file
    results <- results + outer(sign(imp),-1:1,'==')
    i <- i+1
  }
  
  ## Print results
  rownames(results) <- levels(mod$From)
  colnames(results) <- c('-','0','+')
  
  ##save results
  #outname<-results
  write.csv(results, paste(outname,".csv", sep=""))
  
  png(file =paste(outname,".png", sep=""), width = 150, height = 150, units = "mm", res = 600)  
  ## Plot outcomes
  library(RColorBrewer)
  pal <- brewer.pal(n=5,"Greys")[5:1]
  opar <- par(mar=c(5,10,1,1)+0.1)
  prop <- results/rowSums(results)
  r <- colSums(t(prop)*(-1:1))
  barplot(t(prop[order(r),]),
          horiz=T,cex.names=0.8,cex.axis=0.8,las=2,border=T,col=pal,xlab="Proportion")
  par(opar)
  
  
  
  dev.off()
  
} # end function

## RUNS BELOW DONE AT NCEAS, SEE REDO for atf3el and GOAall2el
## final mods use poll2el, atf2el, hal1el, GOAall1el
## rename GOApoll, GOAatf, GOAhal, GOAall
## scenarios "PDO"=+1, "Phytoplankton"=+1, "PSC"=+1, "PSC"=-1
## outnames for files

#without uncertain links
qprPerturbSave(poll2el, c("PDO"=+1), "PDOup_poll2")
qprPerturbSave(poll2el, c("Phytoplankton"=+1), "Phytoup_poll2")
qprPerturbSave(poll2el, c("Halibut_PSC_cap"=+1), "PSCup_poll2")
qprPerturbSave(poll2el, c("Halibut_PSC_cap"=-1), "PSCdown_poll2")

qprPerturbSave(atf2el, c("PDO"=+1), "PDOup_atf2")
qprPerturbSave(atf2el, c("Phytoplankton"=+1), "Phytoup_atf2")
qprPerturbSave(atf2el, c("Halibut_PSC_cap"=+1), "PSCup_atf2")
qprPerturbSave(atf2el, c("Halibut_PSC_cap"=-1), "PSCdown_atf2")

# atf with new environment->pollock links consistent with halibut model
qprPerturbSave(atf3el, c("PDO"=+1), "PDOup_atf3")
qprPerturbSave(atf3el, c("Phytoplankton"=+1), "Phytoup_atf3")
qprPerturbSave(atf3el, c("Halibut_PSC_cap"=+1), "PSCup_atf3")
qprPerturbSave(atf3el, c("Halibut_PSC_cap"=-1), "PSCdown_atf3")


qprPerturbSave(hal1el, c("PDO"=+1), "PDOup_hal1")
qprPerturbSave(hal1el, c("Phytoplankton"=+1), "Phytoup_hal1")
qprPerturbSave(hal1el, c("Halibut_PSC_cap"=+1), "PSCup_hal1")
qprPerturbSave(hal1el, c("Halibut_PSC_cap"=-1), "PSCdown_hal1")

qprPerturbSave(GOAall1el, c("PDO"=+1), "PDOup_merge")
qprPerturbSave(GOAall1el, c("PDO"=-1), "PDOdown_merge")
qprPerturbSave(GOAall1el, c("Phytoplankton"=+1), "Phytoup_merge")
qprPerturbSave(GOAall1el, c("Phytoplankton"=-1), "Phytodown_merge")
qprPerturbSave(GOAall1el, c("Halibut_PSC_cap"=+1), "PSCup_merge")
qprPerturbSave(GOAall1el, c("Halibut_PSC_cap"=-1), "PSCdown_merge")

#merged model with double counted environment->pollock adult links removed
qprPerturbSave(GOAall2el, c("PDO"=+1), "PDOup_merge2")
qprPerturbSave(GOAall2el, c("PDO"=-1), "PDOdown_merge2")
qprPerturbSave(GOAall2el, c("Phytoplankton"=+1), "Phytoup_merge2")
qprPerturbSave(GOAall2el, c("Phytoplankton"=-1), "Phytodown_merge2")
qprPerturbSave(GOAall2el, c("Halibut_PSC_cap"=+1), "PSCup_merge2")
qprPerturbSave(GOAall2el, c("Halibut_PSC_cap"=-1), "PSCdown_merge2")


library(dtplyr)
pollr1<-read.table("PDOup_poll2.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
pollr2<-read.table("Phytoup_poll2.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
pollr3<-read.table("PSCup_poll2.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
pollr4<-read.table("PSCdown_poll2.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

pollr12<-merge(pollr1, pollr2)
pollr34<-merge(pollr3, pollr4)

pollresults<-merge(pollr12, pollr34)

atfr1<-read.table("PDOup_atf2.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
atfr2<-read.table("Phytoup_atf2.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
atfr3<-read.table("PSCup_atf2.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
atfr4<-read.table("PSCdown_atf2.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

atfr12<-merge(atfr1, atfr2)
atfr34<-merge(atfr3, atfr4)

atfresults<-merge(atfr12, atfr34)

# with revised atf model
atfr1<-read.table("PDOup_atf3.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
atfr2<-read.table("Phytoup_atf3.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
atfr3<-read.table("PSCup_atf3.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
atfr4<-read.table("PSCdown_atf3.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

atfr12<-merge(atfr1, atfr2)
atfr34<-merge(atfr3, atfr4)

atfresults2<-merge(atfr12, atfr34)


halr1<-read.table("PDOup_hal1.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
halr2<-read.table("Phytoup_hal1.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
halr3<-read.table("PSCup_hal1.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
halr4<-read.table("PSCdown_hal1.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

halr12<-merge(halr1, halr2)
halr34<-merge(halr3, halr4)

halresults<-merge(halr12, halr34)

allr1<-read.table("PDOup_merge.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
allr2<-read.table("PDOdown_merge.csv", sep=",", col.names=c("name","PDOdown_neg", "PDOdown_0", "PDOdown_pos"))
allr3<-read.table("Phytoup_merge.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
allr4<-read.table("Phytodown_merge.csv", sep=",", col.names=c("name","Phytodown_neg", "Phytodown_0", "Phytodown_pos"))
allr5<-read.table("PSCup_merge.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
allr6<-read.table("PSCdown_merge.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

allr12<-merge(allr1, allr2)
allr34<-merge(allr3, allr4)
allr56<-merge(allr5, allr6)

allr1234<-merge(allr12, allr34)

allresults<-merge(allr1234, allr56)

# with revisions to merged model

allr1<-read.table("PDOup_merge2.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
allr2<-read.table("PDOdown_merge2.csv", sep=",", col.names=c("name","PDOdown_neg", "PDOdown_0", "PDOdown_pos"))
allr3<-read.table("Phytoup_merge2.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
allr4<-read.table("Phytodown_merge2.csv", sep=",", col.names=c("name","Phytodown_neg", "Phytodown_0", "Phytodown_pos"))
allr5<-read.table("PSCup_merge2.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
allr6<-read.table("PSCdown_merge2.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

allr12<-merge(allr1, allr2)
allr34<-merge(allr3, allr4)
allr56<-merge(allr5, allr6)

allr1234<-merge(allr12, allr34)

allresults2<-merge(allr1234, allr56)


#add lookup for categories to each summary table
pollgroups<-read.csv("polllookup.csv", header=T)
atfgroups<-read.csv("atflookup.csv", header=T)
halgroups<-read.csv("hallookup.csv", header=T)
allgroups<-read.csv("mergedlookup.csv", header=T)

pollresults<-merge(pollgroups, pollresults, by.x="varname", by.y="name")
atfresults<-merge(atfgroups, atfresults, by.x="varname", by.y="name")
halresults<-merge(halgroups, halresults, by.x="varname", by.y="name")
allresults<-merge(allgroups, allresults, by.x="varname", by.y="name")
atfresults2<-merge(atfgroups, atfresults2, by.x="varname", by.y="name")
allresults2<-merge(allgroups, allresults2, by.x="varname", by.y="name")

write.csv(pollresults, "pollsummary.csv")
write.csv(atfresults, "atfsummary2.csv")
write.csv(halresults, "halsummary.csv")
write.csv(allresults, "mergedsummary2.csv")
write.csv(atfresults2, "atfsummary2.csv")
write.csv(allresults2, "mergedsummary2.csv")

#with uncertain links
qprPerturbSave(poll2Uel, c("PDO"=+1), "PDOup_poll2U")
qprPerturbSave(poll2Uel, c("Phytoplankton"=+1), "Phytoup_poll2U")
qprPerturbSave(poll2Uel, c("Halibut_PSC_cap"=+1), "PSCup_poll2U")
qprPerturbSave(poll2Uel, c("Halibut_PSC_cap"=-1), "PSCdown_poll2U")

qprPerturbSave(atf2Uel, c("PDO"=+1), "PDOup_atf2U")
qprPerturbSave(atf2Uel, c("Phytoplankton"=+1), "Phytoup_atf2U")
qprPerturbSave(atf2Uel, c("Halibut_PSC_cap"=+1), "PSCup_atf2U")
qprPerturbSave(atf2Uel, c("Halibut_PSC_cap"=-1), "PSCdown_atf2U")

qprPerturbSave(hal1Uel, c("PDO"=+1), "PDOup_hal1U")
qprPerturbSave(hal1Uel, c("Phytoplankton"=+1), "Phytoup_hal1U")
qprPerturbSave(hal1Uel, c("Halibut_PSC_cap"=+1), "PSCup_hal1U")
qprPerturbSave(hal1Uel, c("Halibut_PSC_cap"=-1), "PSCdown_hal1U")

qprPerturbSave(GOAall1Uel, c("PDO"=+1), "PDOup_mergeU")
qprPerturbSave(GOAall1Uel, c("PDO"=-1), "PDOdown_mergeU")
qprPerturbSave(GOAall1Uel, c("Phytoplankton"=+1), "Phytoup_mergeU")
qprPerturbSave(GOAall1Uel, c("Phytoplankton"=-1), "Phytodown_mergeU")
qprPerturbSave(GOAall1Uel, c("Halibut_PSC_cap"=+1), "PSCup_mergeU")
qprPerturbSave(GOAall1Uel, c("Halibut_PSC_cap"=-1), "PSCdown_mergeU")

library(dtplyr)
pollr1U<-read.table("PDOup_poll2U.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
pollr2U<-read.table("Phytoup_poll2U.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
pollr3U<-read.table("PSCup_poll2U.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
pollr4U<-read.table("PSCdown_poll2U.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

pollr12U<-merge(pollr1U, pollr2U)
pollr34U<-merge(pollr3U, pollr4U)

pollresultsU<-merge(pollr12U, pollr34U)

atfr1U<-read.table("PDOup_atf2U.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
atfr2U<-read.table("Phytoup_atf2U.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
atfr3U<-read.table("PSCup_atf2U.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
atfr4U<-read.table("PSCdown_atf2U.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

atfr12U<-merge(atfr1U, atfr2U)
atfr34U<-merge(atfr3U, atfr4U)

atfresultsU<-merge(atfr12U, atfr34U)

halr1U<-read.table("PDOup_hal1U.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
halr2U<-read.table("Phytoup_hal1U.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
halr3U<-read.table("PSCup_hal1U.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
halr4U<-read.table("PSCdown_hal1U.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

halr12U<-merge(halr1U, halr2U)
halr34U<-merge(halr3U, halr4U)

halresultsU<-merge(halr12U, halr34U)

allr1U<-read.table("PDOup_mergeU.csv", sep=",", col.names=c("name","PDOup_neg", "PDOup_0", "PDOup_pos"))
allr2U<-read.table("PDOdown_mergeU.csv", sep=",", col.names=c("name","PDOdown_neg", "PDOdown_0", "PDOdown_pos"))
allr3U<-read.table("Phytoup_mergeU.csv", sep=",", col.names=c("name","Phytoup_neg", "Phytoup_0", "Phytoup_pos"))
allr4U<-read.table("Phytodown_mergeU.csv", sep=",", col.names=c("name","Phytodown_neg", "Phytodown_0", "Phytodown_pos"))
allr5U<-read.table("PSCup_mergeU.csv", sep=",", col.names=c("name","PSCup_neg", "PSCup_0", "PSCup_pos"))
allr6U<-read.table("PSCdown_mergeU.csv", sep=",", col.names=c("name","PSCdown_neg", "PSCdown_0", "PSCdown_pos"))

allr12U<-merge(allr1U, allr2U)
allr34U<-merge(allr3U, allr4U)
allr56U<-merge(allr5U, allr6U)

allr1234U<-merge(allr12U, allr34U)

allresultsU<-merge(allr1234U, allr56U)

#add lookup for categories to each summary table
pollgroups<-read.csv("polllookup.csv", header=T)
atfgroups<-read.csv("atflookup.csv", header=T)
halgroups<-read.csv("hallookup.csv", header=T)
allgroups<-read.csv("mergedlookup.csv", header=T)

pollresultsU<-merge(pollgroups, pollresultsU, by.x="varname", by.y="name")
atfresultsU<-merge(atfgroups, atfresultsU, by.x="varname", by.y="name")
halresultsU<-merge(halgroups, halresultsU, by.x="varname", by.y="name")
allresultsU<-merge(allgroups, allresultsU, by.x="varname", by.y="name")

write.csv(pollresultsU, "pollsummaryU.csv")
write.csv(atfresultsU, "atfsummaryU.csv")
write.csv(halresultsU, "halsummaryU.csv")
write.csv(allresultsU, "mergedsummaryU.csv")


###################################################################33

# GB model for illustration
setwd("~/Data/Projects/ICES_WGNARS/FinalMentalModeler")
GBmm_v1 <- read.csv("GB_Merged_Model.csv")

data5<-GBmm_v1
GB1<-MM2Qpress(data5)
GB1<-dput(GB1)
GB1Model<-parse.text(GB1)

#doesnt work without this
GB1el<-enforce.limitation(GB1Model)

GB1sim<-system.simulate(1000,GB1el)
impact.barplot(GB1sim)

