library(data.table); library(here)

files <- as.data.table(dir(here::here('WSS28model', 'data neutrals')))
#only use .RData
files <- files[V1 %like% '.RData']

all.results <- c()
for(isim in 1:nrow(files)){
    load(here::here('WSS28model', 'data neutrals', files[isim, ]))
    
    #Create output data.tablereg
    model <- gsub(".*neu(.*)\\..*", "\\1", files[isim, ])
    groups <- dimnames(results)[[1]]
    neg <- results[, 1]
    neu <- results[, 2]
    pos <- results[, 3]
    out <- data.table(Model_name = model,
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
    out[Symbol == 'Neg', Size := Neg + 0.5*Neu]
    out[Symbol == 'Pos', Size := Pos + 0.5*Neu]
    out[Symbol == 'Neu', Size := (-1 * Neg) + Pos]
    out[Symbol == 'Mix', Size := 701]
    
    out[Size > 599 & Size <= 700, Strength := 'Weak']
    out[Size > 700 & Size <= 900, Strength := 'Moderate']
    out[Size > 900, Strength := 'Strong']
    
    #Drop extra columns
    out[, c('Neg', 'Neu', 'Pos', 'Size') := NULL]
    
    #join
    all.results <- rbindlist(list(all.results, out))

}






#Jamie's code

library(reshape2)
library(ggplot2)
library(gridExtra)
require(tidyverse)

WSS28_results_ecosQNM <- readRDS(here::here("data/WSS28_results_ecosQNM.RDS"))


ecoQNMresults<-WSS28_results_ecosQNM %>%
    unite("scenario", Group:Direction, remove=FALSE ) %>%
    unite("Model_name", Model:model_links, remove=FALSE)

ecoQNMresults_long<-ecoQNMresults %>%
    pivot_longer (cols= 8:35, names_to="node" ) %>%
    mutate(sign=value) %>%
    mutate(sign=case_when(sign >= 600 ~ "positive",
                          sign <600 & sign >=400~ "zero",
                          sign <400 ~ "negative")) %>%
    # mutate(fill=value) %>%
    mutate(scaled_value= (abs(value-500)))

# Plot up responses as bubble plots 
dat_fishplus<- ecoQNMresults_long %>% 
    filter(scenario=="Fishery_plus")


p_fishplus<- ggplot(dat_fishplus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black", "white"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black", "white")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )



dat_fishminus<- ecoQNMresults_long %>% 
    filter(scenario=="Fishery_minus")

p_fishminus<- ggplot(dat_fishminus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )


dat_sealminus<- ecoQNMresults_long %>% 
    filter(scenario=="Seal_minus")

p_sealminus<- ggplot(dat_sealminus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )

dat_sealplus<- ecoQNMresults_long %>% 
    filter(scenario=="Seal_plus")

p_sealplus<- ggplot(dat_sealplus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )

dat_smallpelagicsminus<- ecoQNMresults_long %>% 
    filter(scenario=="Small pelagics_minus")

p_smallpelagicsminus<- ggplot(dat_smallpelagicsminus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )

dat_smallpelagicsplus<- ecoQNMresults_long %>% 
    filter(scenario=="Small pelagics_plus")

p_smallpelagicsplus<- ggplot(dat_smallpelagicsplus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )

dat_phytoplanktonminus<- ecoQNMresults_long %>% 
    filter(scenario=="Phytoplankton_minus")

p_phytoplanktonminus<- ggplot(dat_phytoplanktonminus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )
dat_phytoplanktonplus<- ecoQNMresults_long %>% 
    filter(scenario=="Phytoplankton_plus")

p_phytoplanktonplus<- ggplot(dat_phytoplanktonplus)+
    geom_point(aes(y=node, x=Model_name,cex=scaled_value, 
                   pch=sign, color=sign, fill=sign)) + 
    # pch=sign, color=sign, fill=fill)) + 
    scale_color_manual( values=c("paleturquoise4","goldenrod3","black"))+
    scale_shape_manual(values=c(25,24,4)) + 
    scale_fill_manual(values=c("paleturquoise4","goldenrod3", "black")) + 
    
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text (angle=-90, hjust=0) 
    )
