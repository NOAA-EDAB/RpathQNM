#Creating a table to compare agreement between model runs
library(data.table); library(here)

#Load results
load(here::here('data', 'all.results.rda'))

scenarios <- unique(all.results[, Scenario])
models <- unique(all.results[, Model_ID])

agg.table <- c()
for(iscene in 1:length(scenarios)){
    scene <- all.results[Scenario == scenarios[iscene], ]
    for(imod in 1:length(models)){
        mod1 <- scene[Model_ID == models[imod], ]
        i <- 1
        while(i <= length(models)){
            mod2 <- scene[Model_ID == models[i], ]
            
            #Add value to detirmine mix agreement
            mod1[Symbol == 'Mix', mix := Pos - Neg]
            mod2[Symbol == 'Mix', mix := Pos - Neg]
            
            #Create agreement table
            agree <- merge(mod1, mod2, by = c('Scenario', 'Group'))
            
            #Symbols that agree get 1 point
            agree[Symbol.x == Symbol.y, Agreement := 1]
            #Opposite symbols get -1 point
            agree[Symbol.x == 'Pos' & Symbol.y == 'Neg', Agreement := -1]
            agree[Symbol.x == 'Neg' & Symbol.y == 'Pos', Agreement := -1]
            #Mix symbols get half a point if they lean towards the same sign
            agree[Symbol.x == 'Mix' & Symbol.y == 'Pos' & mix.x > 300,  Agreement := 0.5]
            agree[Symbol.x == 'Mix' & Symbol.y == 'Neg' & mix.x < -300, Agreement := 0.5]
            agree[Symbol.y == 'Mix' & Symbol.x == 'Pos' & mix.y > 300,  Agreement := 0.5]
            agree[Symbol.y == 'Mix' & Symbol.x == 'Neg' & mix.y < -300, Agreement := 0.5]
            #Mix symbols lose half a point if they lean towards the opposite sign
            agree[Symbol.x == 'Mix' & Symbol.y == 'Pos' & mix.x < -300, Agreement := -0.5]
            agree[Symbol.x == 'Mix' & Symbol.y == 'Neg' & mix.x > 300,  Agreement := -0.5]
            agree[Symbol.y == 'Mix' & Symbol.x == 'Pos' & mix.y < -300, Agreement := -0.5]
            agree[Symbol.y == 'Mix' & Symbol.x == 'Neg' & mix.y > 300,  Agreement := -0.5]
            #All others set to zero
            agree[is.na(Agreement), Agreement := 0]
            
            agreement <- agree[, sum(Agreement), list(Scenario, Model_ID.x, Model_ID.y)]
            agg.table <- rbindlist(list(agg.table, agreement))
            i <- i + 1
        }
    }
}

usethis::use_data(agg.table, overwrite = T)
