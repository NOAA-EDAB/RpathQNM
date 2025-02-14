#WSS Aggregated Rpath Ecosense
#SML

#Required packages--------------------------------------------------------------
library(here); library(data.table); library(Rpath)

#Load and balance model
load(here('data', 'WSS28.params.Rda'))

#load current biomass/landings
#load(here('data', 'GB_biomass_current.RData'))
#load(here('data', 'GB_landings_current.RData'))

#Load Rpath
load(here('data', 'WSS28.rda'))

#Need to fix GB pedigree file - bigger issue to fix eventually!
WSS28.params$pedigree <- WSS28.params$pedigree[Group != 'Fishery', ]

#Test dynamic run
# GB.scene <- rsim.scenario(GB, GB.params, years = 2014:2113)
# GB.scene$params$NoIntegrate[2:6] <- 0
# GB.testrun <- rsim.run(GB.scene, method = 'AB', years = 2014:2113)
# rsim.plot(GB.testrun, GB.params$model[Type < 3, Group])

#Set up sense runs
all_years <- 1:100
scene <- rsim.scenario(WSS28, WSS28.params, years = all_years)

#Fix No Integrate flags
scene$params$NoIntegrate["Microzoop"] <- 0
scene$params$NoIntegrate["Microflora"] <- 0

#Base run 
WSS28.run <- rsim.run(scene, method = 'AB')
rsim.plot(WSS28.run)


# ----- Set up ecosense generator ----- #######################################
scene$params$BURN_YEARS <- 50
NUM_RUNS <- 4e5
parlist <- as.list(rep(NA, NUM_RUNS))
kept <- rep(NA, NUM_RUNS)

set.seed(123)
for (irun in 1:NUM_RUNS){
  WSS28sense <- copy(scene) 
  # INSERT SENSE ROUTINE BELOW
  parlist[[irun]] <- WSS28sense$params 		# Base ecosim params
  parlist[[irun]] <- rsim.sense(WSS28sense, WSS28.params)	# Replace the base params with Ecosense params  
  #WSS28sense$start_state$Biomass <- parlist[[irun]]$B_BaseRef
  parlist[[irun]]$BURN_YEARS <- 50			# Set Burn Years to 50
  WSS28sense$params <- parlist[[irun]]
  WSS28test <- rsim.run(WSS28sense, method = "AB", years = all_years)
  failList <- which(is.na(WSS28test$end_state$Biomass))
  {if (length(failList)>0)
  {cat(irun,": fail in year ",WSS28test$crash_year,": ",failList,"\n"); kept[irun] <- F; flush.console()}
    else 
    {cat(irun,": success!\n"); kept[irun]<-T;  flush.console()}}
  parlist[[irun]]$BURN_YEARS <- 1
}

# KEPT tells you which ecosystems were kept
KEPT <- which(kept==T)
nkept <- length(KEPT)
nkept
#1153
nkept/NUM_RUNS
#0.0029

WSS28.sense <- parlist[KEPT]
usethis::use_data(WSS28.sense, overwrite = TRUE)


