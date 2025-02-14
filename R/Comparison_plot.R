#comparison plot
library(ggplot2); library(data.table); library(here)

#Load comparison table
load(here::here('data', 'agg.table.rda'))

#Change V1 to percentage
agg.table[, Agreement := V1 / 28]

scenes <- unique(agg.table[, Scenario])
all.scenes <- c()
for(iscene in 1:length(scenes)){
    #make the scenario outputs wide
    scene.mat <- data.table::dcast(agg.table[Scenario == scenes[iscene], 
                                         c('Model_ID.x', 'Model_ID.y', 'Agreement')],
                               Model_ID.x ~ Model_ID.y, value.var = 'Agreement')
    
    #Clean up matrix to identify lower triangle
    scene.mat[, Model_ID.x := NULL]
    lower_triangle <- which(lower.tri(scene.mat, diag = TRUE), arr.ind = TRUE)
    
    #Pull data together
    scene.data <- unlist(as.vector(scene.mat))
    
    #For plot
    scene.plot <- data.table(scenario = scenes[iscene],
                             row = lower_triangle[, 1],
                             col = lower_triangle[, 2],
                             value = scene.data[lower.tri(scene.mat, diag = TRUE)])
    
    all.scenes <- data.table::rbindlist(list(all.scenes, scene.plot))
}

# Heatmap 
ggplot(all.scenes, 
       aes(x = col, y = row, fill= value)) + 
    geom_tile() +
    scale_x_discrete(limits = unique(agg.table[, Model_ID.x])) +
    scale_y_reverse(breaks = 1:7, labels = (unique(agg.table[, Model_ID.y]))) +
    scale_fill_viridis_c(direction = -1) +
    facet_wrap(~scenario, nrow = 4) +
    labs(x = 'Model ID', y = 'Model ID')


ggsave(here::here('output', 'Comparison_lower.png'), height = 15, width = 10)    
