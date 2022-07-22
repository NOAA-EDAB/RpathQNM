#Perturbation runs
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
baserun <- rsim.run(scene, method = 'AB')
rsim.plot(baserun)


#Test Force Bio
run.scene <- copy(scene)
run.scene <- adjust.forcing(run.scene, parameter = 'ForcedBio', 
                            group = 'Phytoplankton', sim.year = 11:100, value = 10)
run2 <- rsim.run(run.scene, years = all_years, method = 'AB')
rsim.plot(run2)

#Load sense parameter set
load(here('data', 'WSS28.sense.rda'))
#Will only use the first 1000 parameter sets

#Phytoplankton
phyto.output <- c()
set.seed(123)
#Set-up scenario
#base
base.scene <- rsim.scenario(WSS28, WSS28.params, years = all_years)

for(irun in 1:1000){
    #Set-up scenario
    #base
    run.scene <- copy(base.scene)
    
    #Import sense parameters
    run.scene$params <- WSS28.sense[[irun]]
    #Fix No Integrate flags
    run.scene$params$NoIntegrate["Microzoop"] <- 0
    run.scene$params$NoIntegrate["Microflora"] <- 0
    
    # Run without a perturbation to get the biomass value
    run1 <- rsim.run(run.scene, method = 'AB', years = all_years)
    
    #Diagnostic plots
    plot(run1$annual_Biomass[, 26])
    rsim.plot(run1)
    
    #Calculate baseline biomass values
    bio <- as.data.table(run1$annual_Biomass)
    bio.mean.base <- bio[41:50, lapply(.SD, mean), .SDcols = names(bio)]
    
    #Adjust biomass by 10%
    #Grab reference biomass value (group 26 is Phytoplankton)
    bio.ref <- bio.mean.base[, 26]
    
    pert.scene <- copy(run.scene)
    pert.scene <- adjust.forcing(pert.scene, parameter = 'ForcedBio', 
                                 group = 'Phytoplankton', sim.year = 11:100, 
                                 value = as.numeric(bio.ref + bio.ref * 0.1))
    run2 <- rsim.run(pert.scene, years = all_years, method = 'AB')
    
    #Diagnostic plots
    plot(run2$annual_Biomass[, 26])
    rsim.plot(run2)
    
    #Calculate perturbed biomass
    bio <- as.data.table(run2$annual_Biomass)
    bio.mean.2 <- bio[41:50, lapply(.SD, mean), .SDcols = names(bio)]
    
    #Save difference between base and perturbed
    run.output <- bio.mean.2 - bio.mean.base
    
    #Counter
    cat("Model", irun,": processed\n")
    flush.console()
    phyto.output <- rbindlist(list(phyto.output, run.output))
}

phyto.output[, lapply(.SD, function(x) sum(x > 0))]  






