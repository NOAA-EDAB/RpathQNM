#comparison plot
library(ggplot2); library(data.table); library(here)

#Load comparison table
load(here::here('data', 'agg.table.rda'))

#Change V1 to percentage
agg.table[, Agreement := V1 / 28]

# Heatmap 
ggplot(agg.table, 
       aes(x = Model_ID.x, y = Model_ID.y, fill= Agreement)) + 
    geom_tile() +
    scale_x_discrete(limits = unique(rev(agg.table[, Model_ID.x]))) +
    scale_fill_viridis_c(direction = -1) +
    facet_wrap(~Scenario) +
    labs(x = 'Model ID', y = 'Model ID')
    
