library(data.table); library(here)

#QNM results
files <- as.data.table(dir(here::here('WSS28model', 'data neutrals')))
#only use .RData
files <- files[V1 %like% '.RData']

all.results <- c()
for(isim in 1:nrow(files)){
    load(here::here('WSS28model', 'data neutrals', files[isim, ]))
    
    #Create output data.tablereg
    model <- gsub(".*neu(.*)\\..*", "\\1", files[isim, ])
    model_ID <- paste('QNM_', substr(model, 1, 1), 0, sep = '')
    scenario <- substr(model, 2, 100)
    groups <- dimnames(results)[[1]]
    neg <- results[, 1]
    neu <- results[, 2]
    pos <- results[, 3]
    out <- data.table(Model_ID = model_ID,
                      Scenario = scenario,
                      Group = groups,
                      Neg = neg,
                      Neu = neu,
                      Pos = pos)
    
    #Assign Negative, Neutral, Positive, or Mixed result
    out[Neg > 600, Symbol := 'Neg']
    out[Neu > 600, Symbol := 'Neu']
    out[Pos > 600, Symbol := 'Pos']
    out[is.na(Symbol), Symbol := 'Mix']
    
    #Assign Strength of dominant direction
    out[Symbol == 'Neg', Size := Neg + 0.5 * Neu]
    out[Symbol == 'Pos', Size := Pos + 0.5 * Neu]
    out[Symbol == 'Neu', Size := 701]
    out[Symbol == 'Mix', Size := 701]
    
    out[Size > 599 & Size <= 700, Strength := 'Weak']
    out[Size > 700 & Size <= 900, Strength := 'Moderate']
    out[Size > 900, Strength := 'Strong']
    
    #Drop extra columns
    out[, c('Neg', 'Neu', 'Pos') := NULL]
    
    #join
    all.results <- rbindlist(list(all.results, out))

}

#Change QNM_60 to QNM_0
all.results[Model_ID == 'QNM_60', Model_ID := 'QNM_0']

#Ecosense results
load(here::here('data', 'WSS28.results.rda'))

groups <- colnames(WSS28.results)[which(!colnames(WSS28.results) %in% 
                                            c('Group', 'Direction', 'Outcome', 'Outside'))]

#Clean up names to match QNM
WSS28.results[Direction == 'Plus',  Direction := 'up']
WSS28.results[Direction == 'Minus', Direction := 'down']
WSS28.results[Group == 'Small pelagics', Group := 'Spelagics']

for(isp in 1:length(groups)){
    sp.result <- data.table::dcast(WSS28.results, Group + Direction ~ Outcome, 
                                   value.var = groups[isp])
    sp.result[, Model_ID := 'Rpath']
    sp.result[, Scenario := paste(Group, Direction, sep = '_')]
    sp.result[, Group := NULL]
    sp.result[, Group := groups[isp]]
    
    #Assign Negative, Neutral, Positive, or Mixed result
    sp.result[Negative > 600, Symbol := 'Neg']
    sp.result[Neutral  > 600, Symbol := 'Neu']
    sp.result[Positive > 600, Symbol := 'Pos']
    sp.result[is.na(Symbol), Symbol := 'Mix']
    
    #Assign Strength of dominant direction
    sp.result[Symbol == 'Neg', Size := Negative + 0.5 * Neutral]
    sp.result[Symbol == 'Pos', Size := Positive + 0.5 * Neutral]
    sp.result[Symbol == 'Neu', Size := 701]
    sp.result[Symbol == 'Mix', Size := 701]
    
    sp.result[Size > 599 & Size <= 700, Strength := 'Weak']
    sp.result[Size > 700 & Size <= 900, Strength := 'Moderate']
    sp.result[Size > 900, Strength := 'Strong']
    
    #Drop extra columns
    sp.result[, c('Negative', 'Neutral', 'Positive', 'Direction') := NULL]
    
    #join
    all.results <- rbindlist(list(all.results, sp.result))
}

data.table::setkey(all.results, 'Model_ID', 'Scenario', 'Group')                      

usethis::use_data(all.results, overwrite = T)


