## Western Scotian Shelf model
#-------------------------------------------------------------------------------
#Required packages
library(data.table); library(Rpath); library(here); library(readxl)

#-------------------------------------------------------------------------------
#User created functions

#-------------------------------------------------------------------------------
#Western Scotian Shelf Groups
groups <- c('Whales', 'Toothed cetaceans', 'Seals', 'Sea birds', 'Sharks', 
            'Large pelagic', 'Cod <1', 'Cod 1-3', 'Cod 4-6', 'Cod 7+', 
            'S Hake <25', 'S Hake 25-31', 'S Hake 31+', 'Halibut <46', 'Halibut 46-81',
            'Halibut 82+', 'Pollock <49', 'Pollock 49+', 'D piscvores <40', 'D piscvores 40+',
            'L benthivores <41', 'L bentivores 41+', 'Skates <49', 'Skates 49+',
            'Dogfish', 'Redfish <22', 'Redfish 22+', 'A plaice <26', 'A plaice 26+',
            'Flounders <30', 'Flounders 30+', 'Haddock <3', 'Haddock 3+', 'L sculpin <25',
            'L sculpin 25+', 'Herring <4', 'Herring 4+', 'Other pelagic', 'Mackerel',
            'Mesopelagic', 'Small-medium benthivores', 'Squids', 'Lobster', 'Large crabs',
            'Small crabs', 'Shrimps', 'Scallop', 'Bivalves', 'Other molluscs', 
            'Other arthropoda', 'Echinoderms', 'Sessile benthic groups', 'Worms', 
            'Meiofauna','Gelatinous zoop', 'Macrozoop', 'Mesozoop', 'Microzoop',
            'Microflora', 'Phytoplankton', 'Discards', 'Detritus', 'Fleet1')

#Identify type (0 = consumer, 1 = producer, 2 = detritus, 3 = fleet)
types <- c(rep(0, 59), 1, 2, 2, 3)

#Identify stanza groups
stgroups <- c(rep(NA, 6), rep('Cod', 4), rep('S hake', 3), rep('Halibut', 3), 
              rep('Pollock', 2), rep('D piscvores', 2), rep('L benthivores', 2), 
              rep('Skates', 2), NA, rep('Redfish', 2), rep('A plaice', 2), 
              rep('Flounders', 2), rep('Haddock', 2), rep('S sculpin', 2),
              rep('Herring', 2), rep(NA, 26))

#Create parameter list object
WSS.params <- create.rpath.params(groups, types, stgroups)

#Fill in model parameters
#Biomass, Production, consumption
biomass <- c(0.407075, 0.049754, 0.04383, 0.00617, 0.0260249, 0.023573, rep(NA, 3),
             0.076371, rep(NA, 2), 0.0473429, rep(NA, 2), 0.017291, NA, 0.154529,
             NA, 0.26641, NA, 0.071238, NA, 0.073658, 2.102007, NA, 1.523083, NA,
             0.101483, NA, 0.25631, NA, 0.806943, NA, 0.073649, NA, 2.25021, 0.86577,
             0.535679, 0.012515, 0.037782, 0.173893, 0.301295, 0.157303, 0.965481,
             1.302563, 0.646857, 63.37292, 2, 0.853, 12.63734, 26.02519, 7.345485,
             4.09129, 0.5204, 41.31, 23.79555, 5.800508, 3.678861, 33.664, 0.063293,
             1, NA)

pb <- c(0.071, 0.18, 0.147007, 0.25, 0.18, 0.4, 1.55, 0.39, 0.82, 0.82, 1.6, 1.3,
        1.3, 0.57, 0.27, 0.27, 0.47, 0.85, 0.44, 0.62, 0.62, 0.64, 0.2, 0.26, 0.1393199,
        0.27, 0.23, 0.44, 0.52, 0.53, 0.59, 0.66, 0.42, 0.6, 0.5, 0.64, 0.81, 0.74,
        0.583, 0.97, 1.41, 4, 0.91, 0.654, 1.31, 3, 0.342, 0.69, 0.75, 4.4, 0.33,
        0.32, 1.25, 10.8, 15.51, 3.04, NA, 82.8, 104.938, 70.639, rep(NA, 3))

qb <- c(4.94, 14.5, 7.338632, 87.6, 4.78, 4.24, rep(NA, 3), 1.326, rep(NA, 2), 3.28,
        rep(NA, 2), 1.61, NA, 2.94, NA, 2.5, NA, 1.92, NA, 1.9, 1.739802, NA, 2.41,
        NA, 2.41, NA, 3.21, NA, 2.08, NA, 3.93, NA, 2.36, 3.31, 5.08, 18.9, 4.7,
        11.33333, rep(NA, 12), 62.05, 19.5, 73, rep(NA, 6))

WSS.params$model[, Biomass := biomass]
WSS.params$model[, PB      := pb]
WSS.params$model[, QB      := qb]

#Production to Consumption
WSS.params$model[Group %in% c('Lobster', 'Large crabs', 'Small crabs', 'Shrimps', 
                              'Scallop', 'Bivalves', 'Other molluscs', 'Other arthropoda', 
                              'Echinoderms', 'Sessile benthic groups', 'Worms', 
                              'Meiofauna'),
                 ProdCons := 0.15]
WSS.params$model[Group == 'Mesozoop', ProdCons := 0.4]
WSS.params$model[Group %in% c('Microzoop', 'Microflora'), ProdCons := 0.5]

#Biomass accumulation and unassimilated production
# ba <- c(0.003257, 0, 0.003702, 0, -0.001952, 0, -0.000180536, -0.0111684, -0.01359565,
#         -0.00381855, rep(0, 6), -0.005120684, -0.00618116, -0.00398017, -0.0133205,
#         -0.008473957, -0.0071238, rep(0, 5), -0.002998298, -0.00710381, 0, 0, 
#         0.02020396, 0.05648601, -0.003061481, -0.00368245, rep(0, 7), 0.0301295,
#         rep(0, 3), 0.03234285, rep(0, 13), rep(NA, 3))
#Needed to turn off biomass accumulation for stanza groups as it is already baked in
ba <- c(0.003257, 0, 0.003702, 0, -0.001952, rep(0, 37), 0.0301295,
        rep(0, 3), 0.03234285, rep(0, 13), rep(NA, 3))
WSS.params$model[, BioAcc  := ba]
WSS.params$model[, Unassim := c(rep(0.2, 55), rep(0.4, 3), 0.2, rep(0, 3), NA)]

#Detrital fate
WSS.params$model[, Detritus := c(rep(1, 60), rep(0, 3))]
WSS.params$model[, Discards := c(rep(0, 62), 1)]

#Landings/Discards
land <- c(rep(0, 4), 0.00209077, 0.005067847, 0, 0.024145, 0.08195, 0.010009, 
          0.005438, 0.023001, 0.008442, 2.9E-05, 0.00025, 0.00299, 0.004407,
          0.092313, 0, 0.058946, 0, 0.004406, 0, 0.000799, 0.01325133, 0,
          0.05914, 0, 0.005274, 0, 0.021106, 0.000433, 0.089896, 0.00084, 0.001181,
          0.230954, 0.719899, 0.019708, 0.048208, 0, 0, 0.001637, 0.203756,
          0.016668, 0, 0.000162, 0.106389, 0.010353, 0.00236, 0, 0.03026, 
          rep(0, 11), NA)
disc <- c(0, 0.002995, 0, 2.4E-05, 0.000575, 0.000469, 0.000286, rep(0, 3), 9.6E-05,
          0, 0, 3.2E-05, 0.000274, 0, 0.00029, 0, 0.002805, 0, 0.000385, 0,
          0.003928, 0.009166, 0.009694, 0.00027, 0, 9E-06, 0, 0.000372, 0, 0.000111,
          0, 0.000367, 0.000516, 0.000211, 0, 0.00056, 0.000276, 0, 0.000236,
          1.9E-05, 0.001526, 0.00725, 0.000633, 8E-06, 0, 0, 4.1E-05, 0, 0.016926,
          0.002943, rep(0, 10), NA)
WSS.params$model[, Fleet1      := land]
WSS.params$model[, Fleet1.disc := disc]

#Diet composition
#read DC from EwE6.6
DC.groups <- groups[which(!groups %in% c('Discards', 'Detritus', 'Fleet1'))]
DC.data <- as.data.table(read.csv(here('data-raw', 'WSS and BOF 11-Diet composition.csv')))
setnames(DC.data, names(DC.data), c('X', 'Group', DC.groups))
DC.data[, c('X', 'Phytoplankton') := NULL]
DC.data <- DC.data[1:63, ]
DC.data[, Phytoplankton := as.numeric(0)]

#Merge DC matrix with diet parameter object
# WSS.params$diet <- cbind(WSS.params$diet[, Group], DC.data)
# #Fix column names
# setnames(WSS.params$diet, 1, 'Group')
# setnames(WSS.params$diet, paste0('V', 1:60), DC.groups)
WSS.params$diet <- DC.data

#Set multistanza parameters
#Group parameters
WSS.params$stanzas$stgroups[, VBGF_Ksp := c(0.14, 0.46, 0.14, 0.16, 0.075, 0.14, 
                                            0.13, 0.06, 0.12, 0.21, 0.27, 0.12, 
                                            0.28)]
WSS.params$stanzas$stgroups[, Wmat     := c(0.04, 0.22, 0.24, 0.13, 0.02, 0.15,
                                            0.4, 0.06, 0.06, 0.17, 0.16, 0.15,
                                            0.33)]
WSS.params$stanzas$stgroups[, BAB      := c(-0.05, 0, 0, -0.04, -0.05, -0.10, 0,
                                            0, -0.07, 0, 0.07, -0.05, 0)]
#Individual stanza parameters
WSS.params$stanzas$stindiv[, First   := c(0, 12, 48, 84, 0, 25, 48, 0, 36, 72,
                                          0, 48, 0, 47, 0, 66, 0, 109, 0, 108, 
                                          0, 57, 0, 46, 0, 36, 0, 65, 0, 48)]
WSS.params$stanzas$stindiv[, Last    := c(11, 47, 83, 400, 24, 47, 400, 35, 71,
                                          400, 47, 400, 46, 400, 65, 400, 108, 
                                          400, 107, 400, 56, 400, 45, 400, 35, 
                                          400, 64, 400, 47, 400)]
WSS.params$stanzas$stindiv[, Z       := c(1.55, 0.39, 0.82, 0.82, 1.6, 1.3, 1.3,
                                          0.57, 0.27, 0.27, 0.47, 0.85, 0.44, 0.62,
                                          0.62, 0.64, 0.2, 0.26, 0.27, 0.23, 0.44, 
                                          0.52, 0.53, 0.59, 0.66, 0.42, 0.6, 0.5,
                                          0.64, 0.81)]
WSS.params$stanzas$stindiv[, Leading := c(F, F, F, T, F, F, T, F, F, T, rep(c(F, T), 10))]

#Need to run this function to calculate all the stanza biomass/consumptions
#The calculated biomasses are different than EwE because of the BA terms...will investigate further
#Removed BioAcc for stanza groups and this works close enough (rounding errors)
WSS.params <- rpath.stanzas(WSS.params)

#pedigrees
ped <- data.table::as.data.table(read.csv(here::here('data-raw', 'Pedigree_Full.csv')))

WSS.params$pedigree[, 2:6] <- ped[, 3:7]
usethis::use_data(WSS.params, overwrite = TRUE)

#Ecopath
WSS <- rpath(WSS.params, 'Western Scotian Shelf')


usethis::use_data(WSS, overwrite = TRUE)
