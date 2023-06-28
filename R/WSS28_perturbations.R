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

#Set-up output
output <- c()
set.seed(123)
#Set-up scenario
#base
base.scene <- rsim.scenario(WSS28, WSS28.params, years = all_years)

for(irun in 1:1000){
    #Use base scenario for comparison and replace with ecosense params 
    run.scene <- copy(base.scene)
    run.scene$params <- WSS28.sense[[irun]]
    
    #Fix No Integrate flags
    run.scene$params$NoIntegrate["Microzoop"] <- 0
    run.scene$params$NoIntegrate["Microflora"] <- 0
    
    # Run without a perturbation to get the biomass value
    base <- rsim.run(run.scene, method = 'AB', years = all_years)
    
    #Diagnostic plots
    #plot(base$annual_Biomass[, 26])
    #rsim.plot(base)
    
    #Calculate baseline biomass values
    bio <- as.data.table(base$annual_Biomass)
    bio.mean.base <- bio[91:100, lapply(.SD, mean), .SDcols = names(bio)]
    
    #Calculate baseline fisheries values
    catch <- as.data.table(base$annual_Catch)
    catch.mean.base <- catch[91:100, lapply(.SD, mean), .SDcols = names(catch)]
    catch.base <- sum(catch.mean.base)
    
    #Run perturbations - Living groups
    Groups <- c('Phytoplankton', 'Small pelagics', 'Seals')
    for(igrp in 1:3){
        #Grab reference biomass value
        bio.ref <- bio.mean.base[, .SD, .SDcol = Groups[igrp]]
        
        #Adjust biomass up by 10%
        plus.scene <- copy(run.scene)
        plus.scene <- adjust.forcing(plus.scene, parameter = 'ForcedBio', 
                                     group = Groups[igrp], sim.year = 11:100, 
                                     value = as.numeric(bio.ref + bio.ref * 0.1))
        plus <- rsim.run(plus.scene, years = all_years, method = 'AB')
        
        #Diagnostic plots
        #plot(plus$annual_Biomass[, 26])
        #rsim.plot(plus)
        #rsim.plot(plus, "Phytoplankton", indplot = T)
        
        #Calculate perturbed biomass
        bio <- as.data.table(plus$annual_Biomass)
        bio.mean.plus <- bio[91:100, lapply(.SD, mean), .SDcols = names(bio)]
        
        #Calculate perturbed catch
        catch <- as.data.table(plus$annual_Catch)
        catch.mean.plus <- catch[91:100, lapply(.SD, mean), .SDcols = names(catch)]
        catch.plus <- sum(catch.mean.plus)
        
        #Save percent difference between base and perturbed
        plus.output <- (bio.mean.plus - bio.mean.base) /bio.mean.base
        plus.output[, Fishery := (catch.plus - catch.base) / catch.base]
        
        #Add meta data
        plus.output[, Group := Groups[igrp]]
        plus.output[, Direction := 'Plus']
        
        #Adjust biomass down by 10%
        neg.scene <- copy(run.scene)
        neg.scene <- adjust.forcing(neg.scene, parameter = 'ForcedBio', 
                                    group = Groups[igrp], sim.year = 11:100, 
                                    value = as.numeric(bio.ref - bio.ref * 0.1))
        neg <- rsim.run(neg.scene, years = all_years, method = 'AB')
        
        #Diagnostic plots
        #plot(neg$annual_Biomass[, 26])
        #rsim.plot(neg)
        
        #Calculate perturbed biomass
        bio <- as.data.table(neg$annual_Biomass)
        bio.mean.neg <- bio[91:100, lapply(.SD, mean), .SDcols = names(bio)]
        
        #Calculate perturbed catch
        catch <- as.data.table(neg$annual_Catch)
        catch.mean.neg <- catch[91:100, lapply(.SD, mean), .SDcols = names(catch)]
        catch.neg <- sum(catch.mean.neg)
        
        #Save percent difference between base and perturbed
        neg.output <- (bio.mean.neg - bio.mean.base) / bio.mean.base
        neg.output[, Fishery := (catch.neg - catch.base) / catch.base]
        
        #Add meta data
        neg.output[, Group := Groups[igrp]]
        neg.output[, Direction := 'Minus']
        
        #Append output
        output <- rbindlist(list(output, plus.output, neg.output))
    }
    
    #Run perturbations - Fishery
    #Adjust effort up by 10%
    plus.scene <- copy(run.scene)
    plus.scene <- adjust.fishing(plus.scene, parameter = 'ForcedEffort', 
                                 group = 'Fishery', sim.year = 11:100, 
                                 value = 1.1)
    plus <- rsim.run(plus.scene, years = all_years, method = 'AB')
    
    #Diagnostic plots
    #plot(plus$annual_Biomass[, 26])
    #rsim.plot(plus)
    
    #Calculate perturbed biomass
    bio <- as.data.table(plus$annual_Biomass)
    bio.mean.plus <- bio[91:100, lapply(.SD, mean), .SDcols = names(bio)]
    
    #Calculate perturbed catch
    catch <- as.data.table(plus$annual_Catch)
    catch.mean.plus <- catch[91:100, lapply(.SD, mean), .SDcols = names(catch)]
    catch.plus <- sum(catch.mean.plus)
    
    #Save percent difference between base and perturbed
    plus.output <- (bio.mean.plus - bio.mean.base) / bio.mean.base
    plus.output[, Fishery := (catch.plus - catch.base) / catch.base]
    
    #Add meta data
    plus.output[, Group := 'Fishery']
    plus.output[, Direction := 'Plus']
    
    #Adjust effort down by 10%
    neg.scene <- copy(run.scene)
    neg.scene <- adjust.fishing(neg.scene, parameter = 'ForcedEffort', 
                                group = 'Fishery', sim.year = 11:100, 
                                value = 0.9)
    neg <- rsim.run(neg.scene, years = all_years, method = 'AB')
    
    #Diagnostic plots
    #plot(plus$annual_Biomass[, 26])
    #rsim.plot(plus)
    
    #Calculate perturbed biomass
    bio <- as.data.table(neg$annual_Biomass)
    bio.mean.neg <- bio[41:50, lapply(.SD, mean), .SDcols = names(bio)]
    
    #Calculate perturbed catch
    catch <- as.data.table(neg$annual_Catch)
    catch.mean.neg <- catch[91:100, lapply(.SD, mean), .SDcols = names(catch)]
    catch.neg <- sum(catch.mean.neg)
    
    #Save percent difference between base and perturbed
    neg.output <- (bio.mean.neg - bio.mean.base) / bio.mean.base
    neg.output[, Fishery := (catch.neg - catch.base) / catch.base]
    
    #Add meta data
    neg.output[, Group := 'Fishery']
    neg.output[, Direction := 'Minus']
    
    #Append output
    output <- rbindlist(list(output, plus.output, neg.output))
    
    #Counter
    cat("Model", irun,": processed\n")
    flush.console()
}

#Summarize output
Groups <- c(Groups, 'Fishery')
Direction <- c('Plus', 'Minus')
WSS28.results <- c()
for(igrp in Groups){
    for(idir in Direction){
        out.scene <- output[Group == igrp & Direction == idir, ]
        #Sum by number of outcomes in a specific direction
        #Set a cut-off for essentially no change
        cutoff <- 0.02
        
        pos <- out.scene[, lapply(.SD, function(x) sum(x > cutoff)), 
                      by = c('Group', 'Direction')]
        neu <- out.scene[, lapply(.SD, function(x) sum(x < cutoff & x > -1 * cutoff)), 
                         by = c('Group', 'Direction')]
        neg <- out.scene[, lapply(.SD, function(x) sum(x < -1 * cutoff)), 
                         by = c('Group', 'Direction')]
        
        #Add outcome to the results
        pos[, Outcome := 'Positive']
        neu[, Outcome := 'Neutral']
        neg[, Outcome := 'Negative']
        
        scene.combo <- rbindlist(list(pos, neu, neg))
        WSS28.results <- rbindlist(list(WSS28.results, scene.combo))
    }
}
setcolorder(WSS28.results, c('Group', 'Direction', 'Outcome'))
  
usethis::use_data(WSS28.results, overwrite = TRUE)





